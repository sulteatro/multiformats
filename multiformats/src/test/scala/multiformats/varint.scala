package multiformats.varint

class VarIntTests extends munit.FunSuite:

  private def byteArray(bytes: Int*): Array[Byte] = Array(bytes*).map(_.toByte)

  private def bigIntToByteArray(bi: BigInt): Array[Byte] =
    val barr = bi.toByteArray
    if barr.length > 1 && barr(0).equals(0.toByte) then barr.drop(1) else barr

  // underlying BigInt -> (decoded long, binary rep, hex rep, encoded bytes)
  private val validVarInts: Map[BigInt, (Long, String, String, Array[Byte])] = Map(
    BigInt(0x01) -> (0x01, "00000001", "0x01", byteArray(1)),
    BigInt(0x7f) -> (0x7f, "01111111", "0x7f", byteArray(127)),
    BigInt(0x8001) -> (0x80, "10000000 00000001", "0x8001", byteArray(-128, 1)),
    BigInt(0xff01) -> (0xff, "11111111 00000001", "0xff01", byteArray(-1, 1)),
    BigInt(0xac02) -> (0x012c, "10101100 00000010", "0xac02", byteArray(-84, 2)),
    BigInt(0x808001) -> (0x4000, "10000000 10000000 00000001", "0x808001", byteArray(-128, -128, 1))
  )

  private val invalidVarInts: Map[BigInt, String] = Map(
    BigInt(-128) -> "Invalid unsigned varint: 'Array(-128)'",
    BigInt("-602092467023756490932223") -> "Unsigned varint from Array(-128, -128, -128, -128, -128, -128, -128, -128, -128, 1) exceeds 9 bytes"
  )

  private val invalidSourceInts: Map[Long, String] = Map(
    Long.MaxValue / 128 + 1 -> "Unsigned varint from '72057594037927936' exceeds 9 bytes",
    Long.MaxValue + 1 -> "Negative integers cannot be encoded as unsigned varints"
  )

  test("VarInt.validated(Array[Byte]|BigInt|Long) returns Right[VarInt] for valid encoded ints"):
    validVarInts.keys.foreach { value =>
      assertEquals(VarInt.validated(bigIntToByteArray(value)).map(_.toBigInt), Right(value))
      assertEquals(VarInt.validated(value).map(_.toBigInt), Right(value))
      assertEquals(VarInt.validated(value.toLong).map(_.toBigInt), Right(value))
    }

  test("VarInt.validated(Array[Byte]|BigInt|Long) returns Left[String] for invalid encoded ints"):
    invalidVarInts.foreach { case (value, message) =>
      assertEquals(VarInt.validated(bigIntToByteArray(value)), Left(message))
      assertEquals(VarInt.validated(value), Left(message))
      if value.toLong == value then
        assertEquals(VarInt.validated(value.toLong), Left(message))
    }

  test("VarInt.ifValid(Array[Byte]|BigInt|Long) returns Some[VarInt] for valid encoded ints"):
    validVarInts.keys.foreach { value =>
      assertEquals(VarInt.ifValid(bigIntToByteArray(value)).map(_.toBigInt), Some(value))
      assertEquals(VarInt.ifValid(value).map(_.toBigInt), Some(value))
      assertEquals(VarInt.ifValid(value.toLong).map(_.toBigInt), Some(value))
    }

  test("VarInt.ifValid(Array[Byte]|BigInt|Long) returns None for invalid encoded ints"):
    invalidVarInts.foreach { case (value, _) =>
      assertEquals(VarInt.ifValid(bigIntToByteArray(value)), None)
      assertEquals(VarInt.ifValid(value), None)
      if value.toLong == value then
        assertEquals(VarInt.ifValid(value.toLong), None)
    }

  test("VarInt(Array[Byte]|BigInt|Long) returns VarInt for valid encoded ints"):
    validVarInts.keys.foreach { value =>
      assertEquals(VarInt(bigIntToByteArray(value)).toBigInt, value)
      assertEquals(VarInt(value).toBigInt, value)
      assertEquals(VarInt(value.toLong).toBigInt, value)
    }

  test("VarInt(Array[Byte]|BigInt|Long) throws for invalid encoded ints"):
    invalidVarInts.foreach { case (value, message) =>
      interceptMessage[VarIntValidationError](message):
        VarInt(bigIntToByteArray(value))
      interceptMessage[VarIntValidationError](message):
        VarInt(value)
      if value.toLong == value then
        interceptMessage[VarIntValidationError](message):
          VarInt(value.toLong)
    }

  test("VarInt.encodeValidated(Array[Byte]|BigInt|Long) returns Right[VarInt] for valid ints"):
    validVarInts.foreach { case (value, (source, _, _, _)) =>
      assertEquals(VarInt.encodeValidated(source).map(_.toBigInt), Right(value))
      assertEquals(VarInt.encodeValidated(BigInt(source)).map(_.toBigInt), Right(value))
      assertEquals(
        VarInt.encodeValidated(bigIntToByteArray(BigInt(source))).map(_.toBigInt),
        Right(value)
      )
    }

  test("VarInt.encodeValidated(Array[Byte]|BigInt|Long) returns Left[String] for invalid ints"):
    invalidSourceInts.foreach { case (source, message) =>
      assertEquals(VarInt.encodeValidated(source), Left(message))
      assertEquals(VarInt.encodeValidated(BigInt(source)), Left(message))
      assertEquals(VarInt.encodeValidated(bigIntToByteArray(BigInt(source))), Left(message))
    }

  test("VarInt.encodeIfValid(Array[Byte]|BigInt|Long) returns Some[VarInt] for valid ints"):
    validVarInts.foreach { case (value, (source, _, _, _)) =>
      assertEquals(VarInt.encodeIfValid(source).map(_.toBigInt), Some(value))
      assertEquals(VarInt.encodeIfValid(BigInt(source)).map(_.toBigInt), Some(value))
      assertEquals(
        VarInt.encodeIfValid(bigIntToByteArray(BigInt(source))).map(_.toBigInt),
        Some(value)
      )
    }

  test("VarInt.encodeIfValid(Array[Byte]|BigInt|Long) returns None for invalid integers"):
    invalidSourceInts.foreach { case (source, _) =>
      assertEquals(VarInt.encodeIfValid(source), None)
      assertEquals(VarInt.encodeIfValid(BigInt(source)), None)
      assertEquals(VarInt.encodeIfValid(bigIntToByteArray(BigInt(source))), None)
    }

  test("VarInt.encode(Array[Byte]|BigInt|Long) returns VarInt for valid integers"):
    validVarInts.foreach { case (value, (source, _, _, _)) =>
      assertEquals(VarInt.encode(source).toBigInt, value)
      assertEquals(VarInt.encode(BigInt(source)).toBigInt, value)
      assertEquals(VarInt.encode(bigIntToByteArray(BigInt(source))).toBigInt, value)
    }

  test("VarInt.encode(Array[Byte]|BigInt|Long) throws for invalid integers"):
    invalidSourceInts.foreach { case (source, message) =>
      interceptMessage[VarIntValidationError](message):
        VarInt.encode(source)
      interceptMessage[VarIntValidationError](message):
        VarInt.encode(BigInt(source))
      interceptMessage[VarIntValidationError](message):
        VarInt.encode(bigIntToByteArray(BigInt(source)))
    }

  private def testFullSequenceResult(
      source: Array[Byte],
      count: Option[Int],
      start: Int,
      expected: (Array[Byte], Array[VarInt], Array[Byte])
  ) =
    val result = VarInt.sequence(source, count, start)
    assertEquals(result._1.toSeq, expected._1.toSeq)
    assertEquals(result._2.toSeq, expected._2.toSeq)
    assertEquals(result._3.toSeq, expected._3.toSeq)

  private def testLeftSequenceResult(
      source: Array[Byte],
      count: Int,
      expected: (Array[VarInt], Array[Byte])
  ) =
    val result = VarInt.sequence(source, count)
    assertEquals(result._1.toSeq, expected._1.toSeq)
    assertEquals(result._2.toSeq, expected._2.toSeq)

  test("VarInt.sequence(Array[Byte], Option[Int], Int) extracts varints from a byte array"):
    // varints: Array(0x01, 0x8001, 0x7f, 0xac02, 0x00)
    val test = byteArray(1, -128, 1, 127, -84, 2, 0)

    // extract the first element
    testFullSequenceResult(
      test,
      Some(1),
      0,
      (byteArray(), Array(VarInt(0x01)), byteArray(-128, 1, 127, -84, 2, 0))
    )
    testLeftSequenceResult(
      test,
      1,
      (Array(VarInt(0x01)), byteArray(-128, 1, 127, -84, 2, 0))
    )

    // extract the first two elements
    val expected = Array(VarInt(0x01), VarInt(0x8001))
    testFullSequenceResult(
      test,
      Some(2),
      0,
      (byteArray(), Array(VarInt(0x01), VarInt(0x8001)), byteArray(127, -84, 2, 0))
    )
    testLeftSequenceResult(
      test,
      2,
      (Array(VarInt(0x01), VarInt(0x8001)), byteArray(127, -84, 2, 0))
    )

    // extract the 2nd and 3rd varints
    testFullSequenceResult(
      test,
      Some(2),
      3,
      (byteArray(1, -128, 1), Array(VarInt(0x7f), VarInt(0xac02)), byteArray(0))
    )

    // extract all elements
    val result: Array[VarInt] = Array(0x01, 0x8001, 0x7f, 0xac02, 0x00).map(VarInt(_))

    testFullSequenceResult(
      test,
      Some(5),
      0,
      (byteArray(), result, byteArray())
    )
    testFullSequenceResult(
      test,
      None,
      0,
      (byteArray(), result, byteArray())
    )
    testLeftSequenceResult(
      test,
      5,
      (result, byteArray())
    )
    assertEquals(VarInt.sequence(test).toSeq, result.toSeq)

    // Final bytes that cannot be converted to varint are left alone
    // (also meaning they are ignored by the extract-all version)
    testFullSequenceResult(
      test :+ 0x80.toByte,
      Some(5),
      0,
      (byteArray(), result, byteArray(0x80))
    )
    testFullSequenceResult(
      test :+ 0x80.toByte,
      None,
      0,
      (byteArray(), result, byteArray(0x80))
    )
    testLeftSequenceResult(
      test :+ 0x80.toByte,
      5,
      (result, byteArray(0x80))
    )
    assertEquals(VarInt.sequence(test :+ 0x80.toByte).toSeq, result.toSeq)

  test("VarInt congruency (=~, !~) is just typed (in)equality"):
    validVarInts.foreach { case (value, (source, _, _, _)) =>
      assert(VarInt(value) =~ VarInt.encode(source))
      assert(VarInt(value) !~ VarInt.encode(source + 1))
    }

  test("VarInt congruency (=~, !~) does not compile for other types (including BigInt)"):
    assertNoDiff(
      compileErrors("""VarInt(0x01) =~ BigInt(0x01)"""),
      """|error:
         |Found:    BigInt
         |Required: multiformats.varint.VarInt
         |VarInt(0x01) =~ BigInt(0x01)
         |                     ^
         |""".stripMargin
    )

    assertNoDiff(
      compileErrors("""VarInt(0x01) !~ Array(1.toByte)"""),
      """|error:
         |Found:    Array[Byte]
         |Required: multiformats.varint.VarInt
         |VarInt(0x01) !~ Array(1.toByte)
         |                    ^
         |""".stripMargin
    )

  test("varInt.toBytes returns the bytes encoded by this varint"):
    validVarInts.foreach { case (value, (_, _, _, bytes)) =>
      assertEquals(VarInt(value).toBytes.toSeq, bytes.toSeq)
    }

  test("varInt.toBigInt converts the encoded varint to BigInt"):
    validVarInts.foreach { case (value, (_, _, _, _)) =>
      assertEquals(VarInt(value).toBigInt, value)
    }

  test("varInt.toLong converts the encoded varint to Long"):
    validVarInts.foreach { case (value, (_, _, _, _)) =>
      assertEquals(VarInt(value).toLong, value.toLong)
    }

  test("varInt.toInt converts the encoded varint to Int"):
    validVarInts.foreach { case (value, (_, _, _, _)) =>
      assertEquals(VarInt(value).toInt, value.toInt)
    }

  test("varInt.toHex encodes the varint as a hex string"):
    validVarInts.foreach { case (value, (_, _, hex, _)) =>
      assertEquals(VarInt(value).toHex, hex)
    }

  test("varInt.toBinary encodes the varint as a binary string (grouped into octets)"):
    validVarInts.foreach { case (value, (_, binary, _, _)) =>
      assertEquals(VarInt(value).toBinary, binary)
    }

  test("varInt.length returns the number of encoded bytes"):
    validVarInts.foreach { case (value, (_, _, _, bytes)) =>
      assertEquals(VarInt(value).length, bytes.size)
    }

  test("varInt.decode computes the encoded value as a Long"):
    validVarInts.foreach { case (value, (source, _, _, _)) =>
      assertEquals(VarInt(value).decode, source)
    }
