package milletre

import milletre.utils

final case class CodeGenerationError(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object generator:
  val DefaultDataGroup = "all"

  private def escapeIfKeyword(name: String) =
    if Vector("final").contains(name) then s"`$name`" else name

  private def readCSV(
      filename: String,
      separator: String = raw",\s*",
      groupByOpt: Option[Map[String, String] => String] = None
  ): (Array[String], Map[String, Vector[Array[String]]]) =
    val csvData: Vector[Array[String]] =
      utils.readFile(filename).getLines.map(_.split(separator)).toVector

    val (header, data) = (csvData(0).map(_.toLowerCase), csvData.drop(1))
    val groupedData = groupByOpt.fold(Map(DefaultDataGroup -> data)) {
      groupByRecord =>
        data.groupBy(row => groupByRecord(header.zipWithIndex.map((k, i) => k -> row(i)).toMap))
    }
    (header, groupedData)

  def spliceInto(filename: String, task: String, elements: Seq[String]): Unit =
    val fileLines: Vector[String] = utils.readFile(filename).getLines.toVector

    val (startFlag, endFlag) = (s"// $task: begin //", s"// $task: end //")
    val startIndex = fileLines.indexWhere(_.contains(startFlag))
    val endIndex = fileLines.indexWhere(_.contains(endFlag))

    if startIndex < 0 || endIndex < 0 then
      throw CodeGenerationError(s"Splice range for task '$task' not defined in file '$filename'")

    val indentation = fileLines(startIndex).indexOf(startFlag)

    val code = fileLines.take(startIndex + 1).mkString("\n")
      + "\n"
      + elements.mkString("\n\n").split("\n").map(" ".repeat(indentation) + _).mkString("\n")
      + "\n"
      + fileLines.drop(endIndex).mkString("\n")
      + "\n"

    utils.writeFile(filename, code)

  class CsvCodeGenerator(
      _name: String,
      val source: String,
      groupByOpt: Option[Map[String, String] => String] = None
  ):
    val name: String = _name.toLowerCase
    private lazy val typePrefix = name.capitalize

    case class CsvField(
        name: String,
        typeName: String,
        isEnum: Boolean = false,
        transform: Option[String => String] = None
    ):
      def annotation: String = (if isEnum then typePrefix else "") + typeName

      def format(value: String, asDef: Boolean = false, innerType: Option[String] = None): String =
        val escapedValue = escapeIfKeyword(value)
        transform.map(_(value)).getOrElse(
          innerType.getOrElse(typeName) match
            case "String" =>
              if asDef then escapeIfKeyword(value) else "\"" + value.replace("\"", "\\\"") + "\""
            case s"Option[${inner}]" =>
              if value.isEmpty then "None" else s"Some(${format(value, asDef, Some(inner))})"
            case _ =>
              (if asDef || !isEnum then "" else s"$annotation.") + escapeIfKeyword(value)
        )

    val IndentationSize = 2
    private lazy val indent = " ".repeat(IndentationSize)

    private val (header, groupedData) = readCSV(source, groupByOpt = groupByOpt)

    private def indexedCases(
        field: CsvField,
        asDef: Boolean,
        data: Vector[Array[String]]
    ): Seq[(String, Int)] =
      Option(header.indexWhere(_ == field.name)).filterNot(_ < 0)
        .map(i =>
          data.zipWithIndex.flatMap((row, j) => row.lift(i).map(v => field.format(v, asDef) -> j))
        ).getOrElse(
          throw CodeGenerationError(s"No CSV column '${field.name}' found; options: ${header}")
        )

    def asVector(vecName: String, vecField: CsvField, groupKey: String = DefaultDataGroup): String =
      val valName: String = s"$name${vecName.toLowerCase.capitalize}"
      val vectorDef: String = s"val $valName: Vector[${vecField.annotation}] = Vector("

      val values: Seq[String] =
        indexedCases(vecField, false, groupedData(groupKey)).map(_.head).distinct.sorted
      if values.size < 10 then
        values.mkString(vectorDef, ", ", ")")
      else
        values.map(indent + _).mkString(vectorDef + "\n", ",\n", "\n)")

    def asEnum(
        enumField: CsvField,
        nameOpt: Option[String] = None,
        dataFieldsOpt: Option[Seq[CsvField]] = None,
        groupKey: String = DefaultDataGroup
    ): String =
      val enumName: String =
        s"$typePrefix${nameOpt.getOrElse(enumField.name).toLowerCase.capitalize}"
      val enumDef: String = s"enum ${enumName}" + dataFieldsOpt.fold("") {
        _.map(dataField =>
          if !header.contains(dataField.name) then
            throw CodeGenerationError(
              s"No CSV column '${dataField.name}' found; options: ${header}"
            )
          s"val ${dataField.name}: ${dataField.annotation}"
        ).mkString("(", ", ", ")")
      } + ":"

      val data: Vector[Array[String]] = groupedData(groupKey)
      val indexCases: Seq[(String, Int)] = indexedCases(enumField, true, data)

      val enumCases: Seq[String] = dataFieldsOpt match
        case None =>
          val cases = indexCases.map(_.head).distinct.sorted
          if cases.size < 10 then
            Vector(cases.mkString(indent + "case ", ", ", ""))
          else
            cases.map(c => indent + "case " + c)
        case Some(dataFields) =>
          val indexFields: Seq[(CsvField, Int)] =
            dataFields.map(f => f -> header.indexWhere(_ == f.name)).filterNot(_._2 < 0)

          val nameSpacing: Int = indexCases.map(_._1.size).max

          indexCases.map {
            (caseName, j) =>
              indent
                + "case "
                + caseName.replaceAll("-", "_").padTo(nameSpacing, ' ')
                + s" extends $enumName("
                + indexFields.map((f, i) => f.format(data(j).lift(i).getOrElse(""))).mkString(", ")
                + ")"
          }

      (enumDef +: enumCases).mkString("\n")
