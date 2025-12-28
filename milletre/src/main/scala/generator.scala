package milletre

import scala.io.{Source, BufferedSource}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

final case class CodeGenerationError(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object generator:
  private def readCSV(
      filename: String,
      sep: Option[String] = None
  ): (Array[String], Vector[Array[String]]) =
    val csvSource: BufferedSource =
      if filename.contains("://") then Source.fromURL(filename) else Source.fromFile(filename)
    val csvData: Vector[Array[String]] =
      csvSource.getLines.map(_.split(sep.getOrElse(raw",\s*"))).toVector

    (csvData(0).map(_.toLowerCase), csvData.drop(1))

  case class EnumField(
      name: String,
      typeName: String,
      isEnum: Boolean = false,
      transform: Option[String => String] = None
  ):
    def format(value: String, innerType: Option[String] = None): String =
      transform.map(_(value)).getOrElse(
        innerType.getOrElse(typeName) match
          case "String" => "\"" + value.replace("\"", "\\\"") + "\""
          case s"Option[${inner}]" =>
            if value.isEmpty then "None" else s"Some(${format(value, Some(inner))})"
          case _ if isEnum => s"${typeName}.${value}"
      )

  def spliceInto(filename: String, task: String, elements: Seq[String]): Unit =
    val fileLines: Vector[String] = Source.fromFile(filename).getLines.toVector

    val (startFlag, endFlag) = (s"// $task: begin //", s"// $task: end //")
    val startIndex = fileLines.indexWhere(_.contains(startFlag))
    val indentation = fileLines(startIndex).indexOf(startFlag)
    val endIndex = fileLines.indexWhere(_.contains(endFlag))

    if startIndex < 0 || endIndex < 0 then
      throw CodeGenerationError(s"Splice range for task '$task' not defined in file '$filename'")

    val code = fileLines.take(startIndex + 1).mkString("\n")
      + "\n"
      + elements.mkString("\n\n").indent(indentation)
      + "\n"
      + fileLines.drop(endIndex).mkString("\n")
      + "\n"

    Files.write(Paths.get(filename), code.getBytes(StandardCharsets.UTF_8))

  class EnumGenerator(val sourceFile: String):
    val IndentationSize = 2

    private val (header, data) = readCSV(sourceFile)

    private def columnIndex(col: String): Option[Int] =
      Option(header.indexWhere(_ == col)).filter(_ > -1)

    private def createEnumDef(
        enumName: String,
        optFields: Option[Seq[EnumField]] = None
    ): String =
      s"enum $enumName" + optFields.fold("") {
        _.map(enumField =>
          if !header.contains(enumField.name) then
            throw CodeGenerationError(
              s"No CSV column '${enumField.name}' found; options: ${header}"
            )
          s"val ${enumField.name}: ${enumField.typeName}"
        ).mkString(s"(", ", ", ")") + ":\n"
      }

    private def createEnumCases(
        enumName: String,
        enumCol: String,
        optFields: Option[Seq[EnumField]] = None
    ): String =
      optFields match
        case None =>
          val cases: Seq[String] = columnIndex(enumCol).map(i =>
            data.flatMap(_.lift(i)).distinct.sorted
          ).getOrElse(
            throw CodeGenerationError(s"Enum cases could not be retrieved from '${enumCol}'")
          )
          if cases.size < 10 then
            cases.mkString("case ", ", ", "")
          else
            cases.map(c => s"case $c").mkString("\n")
        case Some(fields) =>
          val indexCases: Seq[(String, Int)] = columnIndex(enumCol).map(i =>
            data.zipWithIndex.flatMap((row, j) => row.lift(i).map(_ -> j))
          ).getOrElse(
            throw CodeGenerationError(s"Enum cases could not be retrieved from '${enumCol}'")
          )
          val indexFields: Seq[(EnumField, Int)] =
            fields.map(f => f -> header.indexWhere(_ == f.name)).filterNot(_._2 < 0)

          val nameSpacing: Int = indexCases.map(_._1.size).max

          indexCases.map {
            (caseName, j) =>
              caseName.replaceAll("-", "_").padTo(nameSpacing, ' ')
                + s" extends $enumName("
                + indexFields.map((f, i) => f.format(data(j).lift(i).getOrElse(""))).mkString(", ")
                + ")"
          }.mkString("case ", "\n", "")

    def apply(enumName: String, enumCol: String, optFields: Option[Seq[EnumField]] = None): String =
      createEnumDef(enumName, optFields)
        + "\n"
        + createEnumCases(enumName, enumCol, optFields).indent(IndentationSize)
