package multiformats.multibase

import milletre.generator
import milletre.utils

@main def generate(): Unit =
  val gen: generator.CsvCodeGenerator = new generator.CsvCodeGenerator(
    "Multibase",
    "https://raw.githubusercontent.com/multiformats/multibase/refs/heads/master/multibase.csv",
    Some(row =>
      row("encoding").toLowerCase match
        case "none" => "reserved"
        case _      => generator.DefaultDataGroup
    )
  )

  val encoding = gen.CsvField("encoding", "String")
  val unicode = gen.CsvField("unicode", "Int", transform = Some(_.replaceAll(raw"U\+", "0x")))
  val description = gen.CsvField("description", "Option[String]")
  val status = gen.CsvField("status", "Status", isEnum = true)

  val multibaseReservedVector = gen.asVector("reserved", unicode, "reserved")
  val multibaseStatusEnumCode = gen.asEnum(status)
  val multibaseEnumCode =
    gen.asEnum(encoding, Some("Algorithm"), Some(Seq(unicode, description, status)))

  val targetFile: String = utils.moduleFile.resolveSibling("parser.scala").toString
  generator.spliceInto(
    targetFile,
    "generateMultibase",
    Vector(multibaseReservedVector, multibaseStatusEnumCode, multibaseEnumCode)
  )
