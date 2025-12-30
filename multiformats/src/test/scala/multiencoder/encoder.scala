package multiencoder

class EncodingTests extends munit.FunSuite:

  private def arrayToString(barr: Array[Int]): String = barr.map(_.toByte.toChar).mkString("")

  private def testByteEncoding(
      name: String,
      enc: encoding.Encoding[?],
      testCases: Map[Array[Byte], String]
  )(using munit.Location): Unit =
    test(s"$name encodes and decodes correctly"):
      testCases.foreach { case (data, encoded) =>
        assertEquals(enc.encode(data), encoded)
        assertEquals(enc.decode(enc.encode(data)).toSeq, data.toSeq)
        assertEquals(enc.decode(encoded).toSeq, data.toSeq)
        assertEquals(enc.encode(enc.decode(encoded)), encoded)
      }

  private def testStringEncoding(
      name: String,
      enc: encoding.Encoding[?],
      testCases: Map[String, String]
  )(using munit.Location): Unit =
    test(s"$name encodes and decodes strings correctly"):
      testCases
        .map((data, encoded) => (data.getBytes, encoded))
        .foreach { case (data, encoded) =>
          assertEquals(enc.encode(data), encoded)
          assertEquals(enc.decode(enc.encode(data)).toSeq, data.toSeq)
          assertEquals(enc.decode(encoded).toSeq, data.toSeq)
          assertEquals(enc.encode(enc.decode(encoded)), encoded)
        }

  testStringEncoding(
    "encoder.Base2",
    encoder.Base2,
    Map(
      "" -> "",
      "f" -> "01100110",
      "fo" -> "0110011001101111",
      "foo" -> "011001100110111101101111",
      "foob" -> "01100110011011110110111101100010",
      "fooba" -> "0110011001101111011011110110001001100001",
      "foobar" -> "011001100110111101101111011000100110000101110010",
      "yes mani !" -> "01111001011001010111001100100000011011010110000101101110011010010010000000100001"
    )
  )

  testStringEncoding(
    "encoder.Base8",
    encoder.Base8,
    Map(
      "" -> "",
      "f" -> "314",
      "fo" -> "314674",
      "foo" -> "31467557",
      "foob" -> "31467557304",
      "fooba" -> "31467557304604",
      "foobar" -> "3146755730460562",
      "foobar?" -> "3146755730460562176",
      "foobar??" -> "3146755730460562176374",
      "foobar???" -> "314675573046056217637477",
      "yes mani !" -> "362625631006654133464440102"
    )
  )

  testStringEncoding(
    "encoder.Base10",
    encoder.Base10,
    Map(
      "" -> "",
      "f" -> "102",
      "fo" -> "26223",
      "foo" -> "6713199",
      "foob" -> "1718579042",
      "fooba" -> "439956234849",
      "foobar" -> "112628796121458",
      "foobar?" -> "28832971807093311",
      "foobar??" -> "7381240782615887679",
      "foobar???" -> "1889597640349667245887",
      "yes mani !" -> "573277761329450583662625"
    )
  )

  // from the RFC
  testByteEncoding(
    "encoder.Base10",
    encoder.Base10,
    Map(
      Array[Byte](0, 1) -> "01",
      Array[Byte](0, 0, 0xff.toByte) -> "00255",
      Array[Byte](1, 0) -> "256",
      Array[Byte](0, 1, 0) -> "0256"
    )
  )

  private val b16Tests: Map[String, String] = Map(
    "" -> "",
    "f" -> "66",
    "fo" -> "666f",
    "foo" -> "666f6f",
    "foob" -> "666f6f62",
    "fooba" -> "666f6f6261",
    "foobar" -> "666f6f626172",
    "foobar?" -> "666f6f6261723f",
    "foobar??" -> "666f6f6261723f3f",
    "foobar???" -> "666f6f6261723f3f3f"
  )

  testStringEncoding(
    "encoder.Base16",
    encoder.Base16,
    b16Tests
  )

  testStringEncoding(
    "encoder.Base16Upper",
    encoder.Base16Upper,
    b16Tests.map { case (k, v) => k -> v.toUpperCase }
  )

  private val b32HexTests: Map[String, (String, Int)] = Map(
    "" -> ("", 0),
    "f" -> ("co", 6),
    "fo" -> ("cpng", 4),
    "foo" -> ("cpnmu", 3),
    "foob" -> ("cpnmuog", 1),
    "fooba" -> ("cpnmuoj1", 0),
    "foobar" -> ("cpnmuoj1e8", 6),
    "foobar?" -> ("cpnmuoj1e8vg", 4),
    "foobar??" -> ("cpnmuoj1e8vju", 3),
    "foobar???" -> ("cpnmuoj1e8vjufo", 1)
  )

  testStringEncoding(
    "encoder.Base32Hex",
    encoder.Base32Hex,
    b32HexTests.map { case (k, (v, _)) => k -> v }
  )

  testStringEncoding(
    "encoder.Base32HexUpper",
    encoder.Base32HexUpper,
    b32HexTests.map { case (k, (v, _)) => k -> v.toUpperCase }
  )

  testStringEncoding(
    "encoder.Base32HexPad",
    encoder.Base32HexPad,
    b32HexTests.map { case (k, (v, p)) => k -> (v + "=" * p) }
  )

  testStringEncoding(
    "encoder.Base32HexPadUpper",
    encoder.Base32HexPadUpper,
    b32HexTests.map { case (k, (v, p)) => k -> (v.toUpperCase + "=" * p) }
  )

  private val b32Tests: Map[String, (String, Int)] = Map(
    "" -> ("", 0),
    "f" -> ("my", 6),
    "fo" -> ("mzxq", 4),
    "foo" -> ("mzxw6", 3),
    "foob" -> ("mzxw6yq", 1),
    "fooba" -> ("mzxw6ytb", 0),
    "foobar" -> ("mzxw6ytboi", 6),
    "foobar?" -> ("mzxw6ytboi7q", 4),
    "foobar??" -> ("mzxw6ytboi7t6", 3),
    "foobar???" -> ("mzxw6ytboi7t6py", 1)
  )

  testStringEncoding(
    "encoder.Base32",
    encoder.Base32,
    b32Tests.map { case (k, (v, _)) => k -> v }
  )

  testStringEncoding(
    "encoder.Base32Upper",
    encoder.Base32Upper,
    b32Tests.map { case (k, (v, _)) => k -> v.toUpperCase }
  )

  testStringEncoding(
    "encoder.Base32Pad",
    encoder.Base32Pad,
    b32Tests.map { case (k, (v, p)) => k -> (v + "=" * p) }
  )

  testStringEncoding(
    "encoder.Base32PadUpper",
    encoder.Base32PadUpper,
    b32Tests.map { case (k, (v, p)) => k -> (v.toUpperCase + "=" * p) }
  )

  testStringEncoding(
    "encoder.Base32Sortable",
    encoder.Base32Sortable,
    Map(
      "" -> "",
      "f" -> "5a",
      "fo" -> "2tnj",
      "foo" -> "agvvj",
      "foob" -> "3nayvv4",
      "fooba" -> "gtrqysn3",
      "foobar" -> "5ahxrq6sfm",
      "foobar?" -> "2tnjhxla4wlz",
      "foobar??" -> "agvvjgdkr6jtz",
      "foobar???" -> "3nayvv4g7t5yjtz"
    )
  )

  testStringEncoding(
    "encoder.Base32z",
    encoder.Base32z,
    Map(
      "" -> "",
      "f" -> "ca",
      "fo" -> "c3zo",
      "foo" -> "c3zs6",
      "foob" -> "c3zs6ao",
      "fooba" -> "c3zs6aub",
      "foobar" -> "c3zs6aubqe",
      "foobar?" -> "c3zs6aubqe9o",
      "foobar??" -> "c3zs6aubqe9u6",
      "foobar???" -> "c3zs6aubqe9u6xa"
    )
  )

  private val b36Tests: Map[String, String] = Map(
    "" -> "",
    "f" -> "2u",
    "fo" -> "k8f",
    "foo" -> "3zvxr",
    "foob" -> "sf742q",
    "fooba" -> "5m42kzfl",
    "foobar" -> "13x8yd7ywi",
    "foobar?" -> "7vwfoe0o75r",
    "foobar??" -> "1k2unhfos2wxr",
    "foobar???" -> "b2s9yzzk7oq81r"
  )

  testStringEncoding(
    "encoder.Base36",
    encoder.Base36,
    b36Tests
  )

  testStringEncoding(
    "encoder.Base36Upper",
    encoder.Base36Upper,
    b36Tests.map { case (k, v) => k -> v.toUpperCase }
  )

  testStringEncoding(
    "encoder.Base58BTC",
    encoder.Base58BTC,
    Map(
      "" -> "",
      "f" -> "2m",
      "fo" -> "8o8",
      "foo" -> "bQbp",
      "foob" -> "3csAg9",
      "fooba" -> "CZJRhmz",
      "foobar" -> "t1Zv2yaZ",
      "foobar?" -> "4t9WH5ij7L",
      "foobar??" -> "J8kYDyqArwx",
      "foobar???" -> "2JdDApHqcXTYr"
    )
  )

  testStringEncoding(
    "encoder.Base58Flickr",
    encoder.Base58Flickr,
    Map(
      "" -> "",
      "f" -> "2L",
      "fo" -> "8N8",
      "foo" -> "ApAP",
      "foob" -> "3BSaF9",
      "fooba" -> "cyiqGLZ",
      "foobar" -> "T1yV2Yzy",
      "foobar?" -> "4T9vh5HJ7k",
      "foobar??" -> "i8KxdYQaRWX",
      "foobar???" -> "2iCdaPhQBwsxR"
    )
  )

  private val b64Tests: Map[String, (String, Int)] = Map(
    "" -> ("", 0),
    "f" -> ("Zg", 2),
    "fo" -> ("Zm8", 1),
    "foo" -> ("Zm9v", 0),
    "foob" -> ("Zm9vYg", 2),
    "fooba" -> ("Zm9vYmE", 1),
    "foobar" -> ("Zm9vYmFy", 0),
    "foobar?" -> ("Zm9vYmFyPw", 2),
    "foobar??" -> ("Zm9vYmFyPz8", 1),
    "foobar???" -> ("Zm9vYmFyPz8/", 0)
  )

  testStringEncoding(
    "encoder.Base64",
    encoder.Base64,
    b64Tests.map { case (k, (v, _)) => k -> v }
  )

  testStringEncoding(
    "encoder.Base64Pad",
    encoder.Base64Pad,
    b64Tests.map { case (k, (v, p)) => k -> (v + "=" * p) }
  )

  testStringEncoding(
    "encoder.Base64Url",
    encoder.Base64Url,
    b64Tests.map { case (k, (v, _)) => k -> v.replace("/", "_") }
  )

  testStringEncoding(
    "encoder.Base64UrlPad",
    encoder.Base64UrlPad,
    b64Tests.map { case (k, (v, p)) => k -> (v.replace("/", "_") + "=" * p) }
  )
