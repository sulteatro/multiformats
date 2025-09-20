import sbt.{AutoPlugin, taskKey}
import io.Source
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths
import scala.util.matching.Regex

object GenerateTasks extends AutoPlugin {
  object autoImport {
    val generateMulticodec = taskKey[Unit]("Generate enums in multicodec.scala from the official table")
    val generateMultibase = taskKey[Unit]("Generate enums in multibase.scala from the official table")
  }
  import autoImport._

  // This means importing the task key in build.sbt is sufficient to enable the plugin 
  override def trigger = allRequirements

  override lazy val projectSettings = Seq(
    generateMulticodec := multicodec.generate(),
    generateMultibase := multibase.generate(),
  )

  private def readCSV(filename: String): (Array[String], Vector[Array[String]]) = {
    val csvData: Vector[Array[String]] =
      Source.fromURL(filename).getLines.map(_.split(raw",\s*")).toVector

    (csvData(0).map(_.toLowerCase), csvData.drop(1))
  }

  private def quoteWrap(str: String): String = "\"" + str.replace("\"", "\\\"") + "\""

  private def writeJson(
    filename: String, data: Seq[Map[String, String]], fields: Seq[String]
  ): Unit = {
    val jsonFields: Seq[(String, String)] = fields.map(f => f -> quoteWrap(f))
    val jsonRecords: Seq[String] = data.map(record =>
      jsonFields
        .flatMap { case (key, jsonField) => record.get(key).map(v => jsonField + ":" + v) }
        .mkString("  {", ",", "}")
    )

    Files.write(
      Paths.get(filename),
      jsonRecords.mkString("[\n", ",\n", "\n]").getBytes
    )
  }

  private def spliceInto(filename: String, task: String, code: String): Unit = {
    val fileLines: Vector[String] = Source.fromFile(filename).getLines.toVector
    val prefix = fileLines.take(fileLines.indexWhere(_.contains(s"// $task: begin //")) + 1).mkString("\n")
    val suffix = fileLines.drop(fileLines.indexWhere(_.contains(s"// $task: end //"))).mkString("\n")

    Files.write(
      Paths.get(filename),
      (prefix + "\n" + code + "\n" + suffix).getBytes(StandardCharsets.UTF_8)
    )
  }

  private def generateBasicEnum(name: String, attrs: Seq[String]): String = {
    val uniqueAttrs: Seq[String] = attrs.toSet.toVector
    if (uniqueAttrs.size < 10) {
      uniqueAttrs.sorted.mkString(s"  enum $name:\n    case ", ", ", "")
    } else {
      uniqueAttrs.sorted.map(attr => s"    case $attr").mkString(s"  enum $name:\n", "\n", "")
    }
  }

  private object multicodec {
    val TaskName: String = "generateMulticodec"
    val EnumName: String = "Multicodec"
    val SourceUrl: String =
      "https://raw.githubusercontent.com/multiformats/multicodec/refs/heads/master/table.csv"
    val FileName: String = "src/main/scala/multiformats/multicodec.scala"

    def generate(): Unit = {
      println(s"Generating multicodec enums in '$FileName' from the official source at '$SourceUrl'")

      val (header, data) = readCSV(SourceUrl)

      val tagsEnum: String = generateBasicEnum(s"${EnumName}Tag", data.map(_(1)))
      val statusEnum: String = generateBasicEnum(s"${EnumName}Status", data.map(_(3)))

      val nameSpacing: Int = data.map(_(0).size).max
      val multicodecEnum: String = (
        s"  enum $EnumName(val ${header(1)}: ${EnumName}Tag, val ${header(2)}: VarInt, val ${header(3)}: ${EnumName}Status, val ${header(4)}: Option[String]):"
          +: data.map { row =>
          val name: String = row(0).replaceAll("-", "_").padTo(nameSpacing, ' ')
          val desc: String = row.lift(4).map(d => s"""Some("$d")""").getOrElse("None")
          s"    case $name extends $EnumName(${EnumName}Tag.${row(1)}, VarInt.encode(${row(2)}), ${EnumName}Status.${row(3)}, $desc)"
        }
      ).mkString("\n")

      spliceInto(FileName, TaskName, Vector(tagsEnum, statusEnum, multicodecEnum).mkString("\n\n"))
    }
  }

  private object multibase {
    val TaskName: String = "generateMultibase"
    val EnumName: String = "BaseAlgorithm"
    val SourceUrl: String =
      "https://raw.githubusercontent.com/multiformats/multibase/refs/heads/master/multibase.csv"
    val FileName: String = "src/main/scala/multiformats/multibase.scala"

    def generate(): Unit = {
      println(s"Generating '$FileName' from the official source at '$SourceUrl'")

      val (header, allEncodings) = readCSV(SourceUrl)
      val (reserved, data) = allEncodings.partition(_(2).equals("none"))

      val reservedVec: String = reserved
        .map(_(0).replaceAll(raw"U\+", "0x"))
        .mkString("  val reserved: Vector[Int] = Vector(", ", ", ")") 
      val statusEnum: String = generateBasicEnum(s"MultibaseStatus", data.map(_(4))).replaceAll("final", "`final`")

      val encodingSpacing: Int = data.map(_(2).size).max
      val multibaseEnum: String = (
        s"  enum ${EnumName}(val ${header(0)}: Int, val ${header(3)}: Option[String], val ${header(4)}: MultibaseStatus):"
          +: data.map { row =>
          val name: String = row(2).padTo(encodingSpacing, ' ')
          val desc: String = row.lift(3).map(d => s"""Some("$d")""").getOrElse("None")
          s"    case $name extends ${EnumName}(${row(0).replaceAll(raw"U\+", "0x")}, $desc, MultibaseStatus.${row(4)})"
        }
      ).mkString("\n").replaceAll("final", "`final`")

      val charFun: String =
        s"""    def character: String = Character.toChars(this.${header(0)}).mkString("")"""

      spliceInto(
        FileName, TaskName, Vector(reservedVec, statusEnum, multibaseEnum, charFun).mkString("\n\n")
      )
    }
  }
}
