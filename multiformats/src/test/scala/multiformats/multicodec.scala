package multiformats.multicodec

import multiformats.varint.VarInt

class MulticodecTests extends munit.FunSuite:
  test("Multicodec.validated(VarInt) returns Right[Multicodec] for a valid code"):
    assertEquals(Multicodec.validated(VarInt.encode(0x00)), Right(Multicodec.identity))
    assertEquals(Multicodec.validated(VarInt.encode(0x0111)), Right(Multicodec.udp))

  test("Multicodec.validated(VarInt) returns Left[String] for an invalid code"):
    assertEquals(
      Multicodec.validated(VarInt(BigInt("18446744073709551487"))),
      Left("Invalid multicodec code: '0xffffffffffffff7f'")
    )

  test("Multicodec.ifValid(VarInt) returns Some[Multicodec] for a valid code"):
    assertEquals(Multicodec.ifValid(VarInt.encode(0x00)), Some(Multicodec.identity))
    assertEquals(Multicodec.ifValid(VarInt.encode(0x0111)), Some(Multicodec.udp))

  test("Multicodec.ifValid(VarInt) returns None for an invalid code"):
    assertEquals(
      Multicodec.ifValid(VarInt(BigInt("18446744073709551487"))),
      None
    )

  test("Multicodec(VarInt) returns Multicodec for a valid code"):
    assertEquals(Multicodec(VarInt.encode(0x00)), Multicodec.identity)
    assertEquals(Multicodec(VarInt.encode(0x0111)), Multicodec.udp)

  test("Multicodec(VarInt) throws for an invalid code"):
    interceptMessage[MulticodecValidationError]("Invalid multicodec code: '0xffffffffffffff7f'"):
      Multicodec(VarInt(BigInt("18446744073709551487")))

  test("Multicodec.validated(String) returns Right[Multicodec] for a valid name"):
    assertEquals(Multicodec.validated("identity"), Right(Multicodec.identity))
    assertEquals(Multicodec.validated("udp"), Right(Multicodec.udp))

  test("Multicodec.validated(String) returns Left[String] for an invalid name"):
    assertEquals(
      Multicodec.validated("INVALID"),
      Left("Invalid multicodec name: 'INVALID'")
    )

  test("Multicodec.validated(String) returns Some[Multicodec] for a valid name"):
    assertEquals(Multicodec.ifValid("identity"), Some(Multicodec.identity))
    assertEquals(Multicodec.ifValid("udp"), Some(Multicodec.udp))

  test("Multicodec.ifValid(String) returns None for an invalid name"):
    assertEquals(
      Multicodec.ifValid("INVALID"),
      None
    )

  test("Multicodec(String) returns Multicodec for a valid name"):
    assertEquals(Multicodec("identity"), Multicodec.identity)
    assertEquals(Multicodec("udp"), Multicodec.udp)

  test("Multicodec(String) throws for an invalid name"):
    interceptMessage[MulticodecValidationError]("Invalid multicodec name: 'INVALID'"):
      Multicodec("INVALID")

  test(
    "Multicodec.validated(Array[Byte]) returns Right[Multicodec] if the array starts with a valid code"
  ):
    assertEquals(Multicodec.validated(Array[Byte](0, 20, 10, 1)), Right(Multicodec.identity))
    assertEquals(Multicodec.validated(Array[Byte](-111, 2, 20, 10, 1)), Right(Multicodec.udp))

  test(
    "Multicodec.validated(Array[Byte]) returns Left[String] if the array does not start with a valid code"
  ):
    assertEquals(
      Multicodec.validated(Array[Byte](-1, -1, -1, -1, -1, -1, -1, 127, 20, 10, 1)),
      Left("Invalid multicodec code: '0xffffffffffffff7f'")
    )

  test(
    "Multicodec.validated(Array[Byte]) returns Right[Multicodec] if the array starts with a valid code"
  ):
    assertEquals(Multicodec.ifValid(Array[Byte](0, 20, 10, 1)), Some(Multicodec.identity))
    assertEquals(Multicodec.ifValid(Array[Byte](-111, 2, 20, 10, 1)), Some(Multicodec.udp))

  test(
    "Multicodec.validated(Array[Byte]) returns Left[String] if the array does not start with a valid code"
  ):
    assertEquals(Multicodec.ifValid(Array[Byte](-1, -1, -1, -1, -1, -1, -1, 127, 20, 10, 1)), None)

  test(
    "Multicodec.validated(Array[Byte]) returns Right[Multicodec] if the array starts with a valid code"
  ):
    assertEquals(Multicodec(Array[Byte](0, 20, 10, 1)), Multicodec.identity)
    assertEquals(Multicodec(Array[Byte](-111, 2, 20, 10, 1)), Multicodec.udp)

  test(
    "Multicodec.validated(Array[Byte]) returns Left[String] if the array does not start with a valid code"
  ):
    interceptMessage[MulticodecValidationError]("Invalid multicodec code: '0xffffffffffffff7f'"):
      Multicodec(Array[Byte](-1, -1, -1, -1, -1, -1, -1, 127, 20, 10, 1))

  test("String context prefix 'mc' returns a Multicodec instance"):
    assertEquals(mc"identity", Multicodec.identity)
    interceptMessage[MulticodecValidationError]("Invalid multicodec name: 'INVALID'"):
      mc"INVALID"
