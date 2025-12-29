package multiformats.multicodec

import milletre.generator.CsvCodeGenerator
import milletre.generator.currentFilePath
import milletre.generator.spliceInto

@main def generate(): Unit =
  val gen: CsvCodeGenerator = new CsvCodeGenerator(
    "Multicodec",
    "https://raw.githubusercontent.com/multiformats/multicodec/refs/heads/master/table.csv"
  )

  val name = gen.CsvField("name", "String")
  val tag = gen.CsvField("tag", "Tag", isEnum = true)
  val code = gen.CsvField("code", "VarInt", transform = Some(v => s"VarInt.encode($v)"))
  val status = gen.CsvField("status", "Status", isEnum = true)
  val description = gen.CsvField("description", "Option[String]")

  val multicodecTagEnumCode = gen.asEnum(tag)
  val multicodecStatusEnumCode = gen.asEnum(status)
  val multicodecEnumCode = gen.asEnum(name, Some(""), Some(Seq(tag, code, status, description)))

  val targetFile: String = currentFilePath.resolveSibling("parser.scala").toString
  spliceInto(
    targetFile,
    "generateMulticodec",
    Vector(multicodecTagEnumCode, multicodecStatusEnumCode, multicodecEnumCode)
  )
