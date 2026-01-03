package milletre

object json:
  def quoteWrap(str: String): String = "\"" + str.replace("\"", "\\\"") + "\""

  def writeRecords(
      filename: String,
      data: Seq[Map[String, String]],
      fields: Seq[String]
  ): Unit =
    val jsonFields: Seq[(String, String)] = fields.map(f => f -> quoteWrap(f))
    val jsonRecords: Seq[String] = data.map(record =>
      jsonFields
        .flatMap { case (key, jsonField) => record.get(key).map(v => jsonField + ":" + v) }
        .mkString("  {", ",", "}")
    )
    val jsonContent: String = jsonRecords.mkString("[\n", ",\n", "\n]")

    utils.writeFile(filename, jsonContent)
