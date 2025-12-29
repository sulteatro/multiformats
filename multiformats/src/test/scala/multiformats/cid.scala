package multiformats.cid

import multiformats.multibase.Multibase
import multiformats.multibase.MultibaseAlgorithm
import multiformats.multibase.MultibaseFactory
import multiformats.multicodec.Multicodec
import multiformats.multicodec.MulticodecIngest
import multiformats.multihash.Multihash
import multiformats.multihash.MultihashAlgorithm
import multiformats.multihash.MultihashFactory
import multiformats.varint.VarInt

class CIDTests extends munit.FunSuite:
  private val cidVersion: Multicodec = Multicodec.cidv1
  private val cidContent = "I'm generating an ID for this content?"
  private val cidContentType: Multicodec = Multicodec.raw
  private val cidMultihashAlgorithm: MultihashAlgorithm = MultihashAlgorithm.sha3_256
  private val cidMultibaseAlgorithm: MultibaseAlgorithm = MultibaseAlgorithm.base32z

  private val cidAddress: Array[Byte] = Array[Byte](
    22, 32, -52, 99, 39, -39, 31, -25, -43, -126, 89, -102, -64, 117, -91, -103, 17, -67, 106, -29,
    78, 94, 99, 107, -79, -114, 96, -8, -2, -22, 122, 5, 0, -80
  )
  private val cidBytes: Array[Byte] = Array[Byte](1, 85) ++ cidAddress

  private val cidMultibase: Multibase = Multibase(
    "hyfktcegcccu718984sbfugsyqs131rp7pmtwhzudpqaaha8a95i8wbeysy"
  )
  private val mhHumanReadable: String =
    "sha3-256-256-cc6327d91fe7d582599ac075a59911bd6ae34e5e636bb18e60f8feea7a0500b0"

  private val cidHumanReadable: String = s"base32z - cidv1 - raw - $mhHumanReadable"

  private val invalidCIDs: Map[Array[Byte], String] = Map(
    cidBytes.updated(0, 5.toByte) -> "Invalid multicodec code: '0x05'",
    cidBytes.updated(0, 4.toByte) -> "Invalid CID multicodec code: '0x04'",
    cidBytes.updated(1, 4.toByte) -> "Invalid content-type multicodec code: '0x04'",
    cidBytes.updated(2, 5.toByte) -> "Invalid multicodec code: '0x05'",
    cidBytes.updated(2, 4.toByte) -> "Unsupported multihash code: '0x04'",
    cidBytes.updated(
      3,
      31.toByte
    ) -> "Mismatch between expected and realized digest sizes: 31 vs 32",
    cidBytes.take(3) -> "Invalid multihash format: could not extract code & size varints",
    Array[Byte](-128) -> "Invalid CID format: could not extract Multicodec code varints"
  )

  private val invalidHumanReadableCIDs: Map[String, String] = Map(
    cidHumanReadable.split(" - ").updated(0, "invalid").mkString(" - ") ->
      "Unsupported multibase codec name: 'invalid'",
    cidHumanReadable.split(" - ").updated(1, "invalid").mkString(" - ") -> (
      "Invalid human-readable CID format: " +
        "'base32z - invalid - raw - sha3-256-256-cc6327d91fe7d582599ac075a59911bd6ae34e5e636bb18e60f8feea7a0500b0'"
    ),
    cidHumanReadable.split(" - ").updated(2, "invalid").mkString(" - ") ->
      "Invalid multicodec name: 'invalid'",
    cidHumanReadable.split(" - ").updated(
      3,
      mhHumanReadable.split("-").updated(0, "invalid").mkString("-")
    ).mkString(" - ") ->
      "Unsupported multihash name: 'invalid-256'",
    cidHumanReadable.split(" - ").updated(
      3,
      mhHumanReadable.split("-").updated(2, "248").mkString("-")
    ).mkString(" - ") ->
      "Mismatch between expected and realized digest sizes: 31 vs 32",
    cidHumanReadable.split(" - ").updated(
      3,
      mhHumanReadable.split("-").updated(3, "ABC*").mkString("-")
    ).mkString(" - ") ->
      "Invalid encoding characters"
  )

  //
  // Converter-factory tests
  //
  test("CID constructors for Array[Byte] return a CID[Raw]-typed byte array"):
    val expected = cidBytes.toSeq

    def testRawCIDConverters(input: Array[Byte]): Unit =
      assertEquals(CID.validated(input).map(_.toBytes.toSeq), Right(expected))
      assertEquals(CID.ifValid(input).map(_.toBytes.toSeq), Some(expected))
      assertEquals(CID(input).toBytes.toSeq, expected)

    testRawCIDConverters(cidBytes)

  test("CID constructors for Array[Byte] perform failure modes for invalid CIDs"):
    def testRawCIDConverterFailures(input: Array[Byte], msg: String): Unit =
      assertEquals(CID.validated(input), Left(msg))
      assertEquals(CID.ifValid(input), None)
      interceptMessage[CIDValidationError](msg):
        CID(input)

    invalidCIDs.foreach { case (value, msg) =>
      testRawCIDConverterFailures(value, msg)
    }

  test("CID constructors for Multibase and String return a CID[Encoded]-typed Multibase object"):
    val expected = cidMultibase.toString

    def testEncodedCIDConverters[I](input: I)(using CIDConverterFactory[I]): Unit =
      assertEquals(CID.validated(input).map(_.toString), Right(expected))
      assertEquals(CID.ifValid(input).map(_.toString), Some(expected))
      assertEquals(CID(input).toString, expected)

    testEncodedCIDConverters(cidMultibase)
    testEncodedCIDConverters(cidMultibase.toString)
    testEncodedCIDConverters(cidHumanReadable)

  test("CID constructors for Multibase and String perform failure modes for invalid CIDs"):
    def testEncodedCIDConverterFailures[I](input: I, msg: String)(using
        CIDConverterFactory[I]
    ): Unit =
      assertEquals(CID.validated(input), Left(msg))
      assertEquals(CID.ifValid(input), None)
      interceptMessage[CIDValidationError](msg):
        CID(input)

    invalidCIDs.foreach { case (value, msg) =>
      testEncodedCIDConverterFailures(Multibase.encode(value, cidMultibaseAlgorithm), msg)
    }

    invalidHumanReadableCIDs.foreach { case (value, msg) =>
      testEncodedCIDConverterFailures(value, msg)
    }

  test("CID constructors for address and content type return a CID[Raw]"):
    val expected = cidBytes.toSeq

    def testRawCIDConstructors[T, A](contentType: T, address: A)(using
        MulticodecIngest[T],
        CIDConstructorFactory[A]
    ): Unit =
      assertEquals(CID.validated(contentType, address).map(_.toBytes.toSeq), Right(expected))
      assertEquals(CID.ifValid(contentType, address).map(_.toBytes.toSeq), Some(expected))
      assertEquals(CID(contentType, address).toBytes.toSeq, expected)

    val (ctCodec, ctCode, ctName) = (cidContentType, cidContentType.code, cidContentType.toString)
    val ctPrefixed = cidBytes.drop(1)

    testRawCIDConstructors(ctCodec, cidAddress)
    testRawCIDConstructors(ctCode, cidAddress)
    testRawCIDConstructors(ctName, cidAddress)
    testRawCIDConstructors(ctPrefixed, cidAddress)

    val mhAddress = Multihash(cidAddress)

    testRawCIDConstructors(ctCodec, mhAddress)
    testRawCIDConstructors(ctCode, mhAddress)
    testRawCIDConstructors(ctName, mhAddress)
    testRawCIDConstructors(ctPrefixed, mhAddress)

  test("CID constructors for address and content type perform failure modes for invalid CIDs"):
    def testRawCIDConstructorFailures[T, A](contentType: T, address: A, msg: String)(using
        MulticodecIngest[T],
        CIDConstructorFactory[A]
    ): Unit =
      assertEquals(CID.validated(contentType, address), Left(msg))
      assertEquals(CID.ifValid(contentType, address), None)
      interceptMessage[CIDValidationError](msg):
        CID(contentType, address)

    val badMC: VarInt = VarInt.encode(0x05)

    testRawCIDConstructorFailures(
      badMC,
      cidAddress,
      "Invalid multicodec code: '0x05'"
    )
    testRawCIDConstructorFailures(
      "invalid",
      cidAddress,
      "Invalid multicodec name: 'invalid'"
    )
    testRawCIDConstructorFailures(
      badMC.toBytes :+ 85.toByte,
      cidAddress,
      "Invalid multicodec code: '0x05'"
    )
    testRawCIDConstructorFailures(
      Array(-127.toByte, -14.toByte),
      cidAddress,
      "Could not extract a varint from bytes"
    )
    testRawCIDConstructorFailures(
      cidContentType,
      badMC.toBytes ++ cidAddress.drop(1),
      "Invalid multicodec code: '0x05'"
    )
    testRawCIDConstructorFailures(
      cidContentType,
      cidAddress.updated(1, 31.toByte),
      "Mismatch between expected and realized digest sizes: 31 vs 32"
    )
    testRawCIDConstructorFailures(
      cidContentType,
      Array(85.toByte),
      "Invalid multihash format: could not extract code & size varints"
    )
    testRawCIDConstructorFailures(
      VarInt.encode(0x04),
      cidAddress,
      "Invalid content-type multicodec code: '0x04'"
    )

  test("CID constructors for address, content type, and base encoding return a CID[Encoded]"):
    val expected = cidMultibase.toString

    def testEncodedCIDConstructors[T, A, B](contentType: T, address: A, base: B)(using
        MulticodecIngest[T],
        CIDConstructorFactory[A],
        MultibaseFactory[Array[Byte], B]
    ): Unit =
      assertEquals(CID.validated(contentType, address, base).map(_.toString), Right(expected))
      assertEquals(CID.ifValid(contentType, address, base).map(_.toString), Some(expected))
      assertEquals(CID(contentType, address, base).toString, expected)

    val mhAddress = Multihash(cidAddress)

    val (ctCodec, ctCode, ctName) = (cidContentType, cidContentType.code, cidContentType.toString)
    val ctPrefixed = cidBytes.drop(1)

    val mbCodec = cidMultibaseAlgorithm
    testEncodedCIDConstructors(ctCodec, cidAddress, mbCodec)
    testEncodedCIDConstructors(ctCode, cidAddress, mbCodec)
    testEncodedCIDConstructors(ctName, cidAddress, mbCodec)
    testEncodedCIDConstructors(ctPrefixed, cidAddress, mbCodec)

    testEncodedCIDConstructors(ctCodec, mhAddress, mbCodec)
    testEncodedCIDConstructors(ctName, mhAddress, mbCodec)
    testEncodedCIDConstructors(ctCode, mhAddress, mbCodec)
    testEncodedCIDConstructors(ctPrefixed, mhAddress, mbCodec)

    val mbName = cidMultibaseAlgorithm.toString
    testEncodedCIDConstructors(ctCodec, cidAddress, mbName)
    testEncodedCIDConstructors(ctCode, cidAddress, mbName)
    testEncodedCIDConstructors(ctName, cidAddress, mbName)
    testEncodedCIDConstructors(ctPrefixed, cidAddress, mbName)

    testEncodedCIDConstructors(ctCodec, mhAddress, mbName)
    testEncodedCIDConstructors(ctName, mhAddress, mbName)
    testEncodedCIDConstructors(ctCode, mhAddress, mbName)
    testEncodedCIDConstructors(ctPrefixed, mhAddress, mbName)

    val mbChar = cidMultibaseAlgorithm.character
    testEncodedCIDConstructors(ctCodec, cidAddress, mbChar)
    testEncodedCIDConstructors(ctCode, cidAddress, mbChar)
    testEncodedCIDConstructors(ctName, cidAddress, mbChar)
    testEncodedCIDConstructors(ctPrefixed, cidAddress, mbChar)

    testEncodedCIDConstructors(ctCodec, mhAddress, mbChar)
    testEncodedCIDConstructors(ctName, mhAddress, mbChar)
    testEncodedCIDConstructors(ctCode, mhAddress, mbChar)
    testEncodedCIDConstructors(ctPrefixed, mhAddress, mbChar)

  test(
    "CID constructors for address, content type, and base encoding perform failure modes for invalid CIDs"
  ):
    def testEncodedCIDConstructorFailures[T, A, B](
        contentType: T,
        address: A,
        base: B,
        msg: String
    )(using
        MulticodecIngest[T],
        CIDConstructorFactory[A],
        MultibaseFactory[Array[Byte], B]
    ): Unit =
      assertEquals(CID.validated(contentType, address, base), Left(msg))
      assertEquals(CID.ifValid(contentType, address, base), None)
      interceptMessage[CIDValidationError](msg):
        CID(contentType, address, base)

    val badMC: VarInt = VarInt.encode(0x05)

    testEncodedCIDConstructorFailures(
      badMC,
      cidAddress,
      cidMultibaseAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testEncodedCIDConstructorFailures(
      "invalid",
      cidAddress,
      cidMultibaseAlgorithm,
      "Invalid multicodec name: 'invalid'"
    )
    testEncodedCIDConstructorFailures(
      badMC.toBytes :+ 85.toByte,
      cidAddress,
      cidMultibaseAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testEncodedCIDConstructorFailures(
      Array(-127.toByte, -14.toByte),
      cidAddress,
      cidMultibaseAlgorithm,
      "Could not extract a varint from bytes"
    )
    testEncodedCIDConstructorFailures(
      cidContentType,
      badMC.toBytes ++ cidAddress.drop(1),
      cidMultibaseAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testEncodedCIDConstructorFailures(
      cidContentType,
      cidAddress.updated(1, 31.toByte),
      cidMultibaseAlgorithm,
      "Mismatch between expected and realized digest sizes: 31 vs 32"
    )
    testEncodedCIDConstructorFailures(
      cidContentType,
      Array(85.toByte),
      cidMultibaseAlgorithm,
      "Invalid multihash format: could not extract code & size varints"
    )
    testEncodedCIDConstructorFailures(
      VarInt.encode(0x04),
      cidAddress,
      cidMultibaseAlgorithm,
      "Invalid content-type multicodec code: '0x04'"
    )
    testEncodedCIDConstructorFailures(
      cidContentType,
      cidAddress,
      "~",
      "Unsupported multibase prefix character: '~'"
    )
    testEncodedCIDConstructorFailures(
      cidContentType,
      cidAddress,
      "invalid",
      "Unsupported multibase codec name: 'invalid'"
    )

  test("CID digestors for content, content type, and hash algorithm return a CID[Raw]"):
    val expected = cidBytes.toSeq

    def testRawCIDDigestors[C, T, H](content: C, contentType: T, hash: H)(using
        CIDDigestorFactory[C],
        MulticodecIngest[T],
        MultihashFactory[H]
    ): Unit =
      assertEquals(
        CID.digestValidated(content, contentType, hash).map(_.toBytes.toSeq),
        Right(expected)
      )
      assertEquals(
        CID.digestIfValid(content, contentType, hash).map(_.toBytes.toSeq),
        Some(expected)
      )
      assertEquals(CID.digest(content, contentType, hash).toBytes.toSeq, expected)

    val contentBytes: Array[Byte] = cidContent.getBytes

    val (ctCodec, ctCode, ctName) = (cidContentType, cidContentType.code, cidContentType.toString)
    val ctPrefixed = cidBytes.drop(1)

    val (haCodec, haCode) = (cidMultihashAlgorithm, cidMultihashAlgorithm.code)
    val (haName, haLabel, haToString) =
      (cidMultihashAlgorithm.name, cidMultihashAlgorithm.label, cidMultihashAlgorithm.toString)

    testRawCIDDigestors(cidContent, ctCodec, haCodec)
    testRawCIDDigestors(cidContent, ctCode, haCodec)
    testRawCIDDigestors(cidContent, ctName, haCodec)
    testRawCIDDigestors(cidContent, ctPrefixed, haCodec)

    testRawCIDDigestors(contentBytes, ctCodec, haCodec)
    testRawCIDDigestors(contentBytes, ctCode, haCodec)
    testRawCIDDigestors(contentBytes, ctName, haCodec)
    testRawCIDDigestors(contentBytes, ctPrefixed, haCodec)

    testRawCIDDigestors(cidContent, ctCodec, haCode)
    testRawCIDDigestors(cidContent, ctCode, haCode)
    testRawCIDDigestors(cidContent, ctName, haCode)
    testRawCIDDigestors(cidContent, ctPrefixed, haCode)

    testRawCIDDigestors(contentBytes, ctCodec, haCode)
    testRawCIDDigestors(contentBytes, ctCode, haCode)
    testRawCIDDigestors(contentBytes, ctName, haCode)
    testRawCIDDigestors(contentBytes, ctPrefixed, haCode)

    testRawCIDDigestors(cidContent, ctCodec, haName)
    testRawCIDDigestors(cidContent, ctCode, haName)
    testRawCIDDigestors(cidContent, ctName, haName)
    testRawCIDDigestors(cidContent, ctPrefixed, haName)

    testRawCIDDigestors(contentBytes, ctCodec, haName)
    testRawCIDDigestors(contentBytes, ctCode, haName)
    testRawCIDDigestors(contentBytes, ctName, haName)
    testRawCIDDigestors(contentBytes, ctPrefixed, haName)

    testRawCIDDigestors(cidContent, ctCodec, haLabel)
    testRawCIDDigestors(cidContent, ctCode, haLabel)
    testRawCIDDigestors(cidContent, ctName, haLabel)
    testRawCIDDigestors(cidContent, ctPrefixed, haLabel)

    testRawCIDDigestors(contentBytes, ctCodec, haLabel)
    testRawCIDDigestors(contentBytes, ctCode, haLabel)
    testRawCIDDigestors(contentBytes, ctName, haLabel)
    testRawCIDDigestors(contentBytes, ctPrefixed, haLabel)

    testRawCIDDigestors(cidContent, ctCodec, haToString)
    testRawCIDDigestors(cidContent, ctCode, haToString)
    testRawCIDDigestors(cidContent, ctName, haToString)
    testRawCIDDigestors(cidContent, ctPrefixed, haToString)

    testRawCIDDigestors(contentBytes, ctCodec, haToString)
    testRawCIDDigestors(contentBytes, ctCode, haToString)
    testRawCIDDigestors(contentBytes, ctName, haToString)
    testRawCIDDigestors(contentBytes, ctPrefixed, haToString)

  test(
    "CID digestors for content, content type, and hash algorithm perform failure modes for invalid CIDs"
  ):
    val expected = cidBytes.toSeq

    def testRawCIDDigestorFailures[T, H](contentType: T, hash: H, msg: String)(using
        MulticodecIngest[T],
        MultihashFactory[H]
    ): Unit =
      assertEquals(CID.digestValidated(cidContent, contentType, hash), Left(msg))
      assertEquals(CID.digestIfValid(cidContent, contentType, hash), None)
      interceptMessage[CIDValidationError](msg):
        CID.digest(cidContent, contentType, hash)

    val badMC: VarInt = VarInt.encode(0x05)

    testRawCIDDigestorFailures(
      badMC,
      cidMultihashAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testRawCIDDigestorFailures(
      "invalid",
      cidMultihashAlgorithm,
      "Invalid multicodec name: 'invalid'"
    )
    testRawCIDDigestorFailures(
      badMC.toBytes :+ 85.toByte,
      cidMultihashAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testRawCIDDigestorFailures(
      Array(-127.toByte, -14.toByte),
      cidMultihashAlgorithm,
      "Could not extract a varint from bytes"
    )
    testRawCIDDigestorFailures(
      cidContentType,
      VarInt.encode(0x04),
      "Unsupported multihash code: '0x04'"
    )
    testRawCIDDigestorFailures(
      cidContentType,
      "invalid",
      "Unsupported multihash name: 'invalid'"
    )

  test(
    "CID digestors for content, content type, hash algorithm, and base encoding return a CID[Encoded]"
  ):
    val expected = cidMultibase.toString

    def testEncodedCIDDigestors[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
        CIDDigestorFactory[C],
        MulticodecIngest[T],
        MultihashFactory[H],
        MultibaseFactory[Array[Byte], B]
    ): Unit =
      assertEquals(
        CID.digestValidated(content, contentType, hash, base).map(_.toString),
        Right(expected)
      )
      assertEquals(
        CID.digestIfValid(content, contentType, hash, base).map(_.toString),
        Some(expected)
      )
      assertEquals(CID.digest(content, contentType, hash, base).toString, expected)

    val contentBytes: Array[Byte] = cidContent.getBytes

    val (ctCodec, ctCode, ctName) = (cidContentType, cidContentType.code, cidContentType.toString)
    val ctPrefixed = cidBytes.drop(1)

    val (haCodec, haCode) = (cidMultihashAlgorithm, cidMultihashAlgorithm.code)
    val (haName, haLabel, haToString) =
      (cidMultihashAlgorithm.name, cidMultihashAlgorithm.label, cidMultihashAlgorithm.toString)

    val mbCodec = cidMultibaseAlgorithm
    testEncodedCIDDigestors(cidContent, ctCodec, haCodec, mbCodec)
    testEncodedCIDDigestors(cidContent, ctCode, haCodec, mbCodec)
    testEncodedCIDDigestors(cidContent, ctName, haCodec, mbCodec)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haCodec, mbCodec)

    testEncodedCIDDigestors(contentBytes, ctCodec, haCodec, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctCode, haCodec, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctName, haCodec, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haCodec, mbCodec)

    testEncodedCIDDigestors(cidContent, ctCodec, haCode, mbCodec)
    testEncodedCIDDigestors(cidContent, ctCode, haCode, mbCodec)
    testEncodedCIDDigestors(cidContent, ctName, haCode, mbCodec)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haCode, mbCodec)

    testEncodedCIDDigestors(contentBytes, ctCodec, haCode, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctCode, haCode, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctName, haCode, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haCode, mbCodec)

    testEncodedCIDDigestors(cidContent, ctCodec, haName, mbCodec)
    testEncodedCIDDigestors(cidContent, ctCode, haName, mbCodec)
    testEncodedCIDDigestors(cidContent, ctName, haName, mbCodec)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haName, mbCodec)

    testEncodedCIDDigestors(contentBytes, ctCodec, haName, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctCode, haName, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctName, haName, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haName, mbCodec)

    testEncodedCIDDigestors(cidContent, ctCodec, haLabel, mbCodec)
    testEncodedCIDDigestors(cidContent, ctCode, haLabel, mbCodec)
    testEncodedCIDDigestors(cidContent, ctName, haLabel, mbCodec)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haLabel, mbCodec)

    testEncodedCIDDigestors(contentBytes, ctCodec, haLabel, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctCode, haLabel, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctName, haLabel, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haLabel, mbCodec)

    testEncodedCIDDigestors(cidContent, ctCodec, haToString, mbCodec)
    testEncodedCIDDigestors(cidContent, ctCode, haToString, mbCodec)
    testEncodedCIDDigestors(cidContent, ctName, haToString, mbCodec)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haToString, mbCodec)

    testEncodedCIDDigestors(contentBytes, ctCodec, haToString, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctCode, haToString, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctName, haToString, mbCodec)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haToString, mbCodec)

    val mbName = cidMultibaseAlgorithm
    testEncodedCIDDigestors(cidContent, ctCodec, haCodec, mbName)
    testEncodedCIDDigestors(cidContent, ctCode, haCodec, mbName)
    testEncodedCIDDigestors(cidContent, ctName, haCodec, mbName)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haCodec, mbName)

    testEncodedCIDDigestors(contentBytes, ctCodec, haCodec, mbName)
    testEncodedCIDDigestors(contentBytes, ctCode, haCodec, mbName)
    testEncodedCIDDigestors(contentBytes, ctName, haCodec, mbName)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haCodec, mbName)

    testEncodedCIDDigestors(cidContent, ctCodec, haCode, mbName)
    testEncodedCIDDigestors(cidContent, ctCode, haCode, mbName)
    testEncodedCIDDigestors(cidContent, ctName, haCode, mbName)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haCode, mbName)

    testEncodedCIDDigestors(contentBytes, ctCodec, haCode, mbName)
    testEncodedCIDDigestors(contentBytes, ctCode, haCode, mbName)
    testEncodedCIDDigestors(contentBytes, ctName, haCode, mbName)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haCode, mbName)

    testEncodedCIDDigestors(cidContent, ctCodec, haName, mbName)
    testEncodedCIDDigestors(cidContent, ctCode, haName, mbName)
    testEncodedCIDDigestors(cidContent, ctName, haName, mbName)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haName, mbName)

    testEncodedCIDDigestors(contentBytes, ctCodec, haName, mbName)
    testEncodedCIDDigestors(contentBytes, ctCode, haName, mbName)
    testEncodedCIDDigestors(contentBytes, ctName, haName, mbName)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haName, mbName)

    testEncodedCIDDigestors(cidContent, ctCodec, haLabel, mbName)
    testEncodedCIDDigestors(cidContent, ctCode, haLabel, mbName)
    testEncodedCIDDigestors(cidContent, ctName, haLabel, mbName)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haLabel, mbName)

    testEncodedCIDDigestors(contentBytes, ctCodec, haLabel, mbName)
    testEncodedCIDDigestors(contentBytes, ctCode, haLabel, mbName)
    testEncodedCIDDigestors(contentBytes, ctName, haLabel, mbName)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haLabel, mbName)

    testEncodedCIDDigestors(cidContent, ctCodec, haToString, mbName)
    testEncodedCIDDigestors(cidContent, ctCode, haToString, mbName)
    testEncodedCIDDigestors(cidContent, ctName, haToString, mbName)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haToString, mbName)

    testEncodedCIDDigestors(contentBytes, ctCodec, haToString, mbName)
    testEncodedCIDDigestors(contentBytes, ctCode, haToString, mbName)
    testEncodedCIDDigestors(contentBytes, ctName, haToString, mbName)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haToString, mbName)

    val mbChar = cidMultibaseAlgorithm.character
    testEncodedCIDDigestors(cidContent, ctCodec, haCodec, mbChar)
    testEncodedCIDDigestors(cidContent, ctCode, haCodec, mbChar)
    testEncodedCIDDigestors(cidContent, ctName, haCodec, mbChar)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haCodec, mbChar)

    testEncodedCIDDigestors(contentBytes, ctCodec, haCodec, mbChar)
    testEncodedCIDDigestors(contentBytes, ctCode, haCodec, mbChar)
    testEncodedCIDDigestors(contentBytes, ctName, haCodec, mbChar)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haCodec, mbChar)

    testEncodedCIDDigestors(cidContent, ctCodec, haCode, mbChar)
    testEncodedCIDDigestors(cidContent, ctCode, haCode, mbChar)
    testEncodedCIDDigestors(cidContent, ctName, haCode, mbChar)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haCode, mbChar)

    testEncodedCIDDigestors(contentBytes, ctCodec, haCode, mbChar)
    testEncodedCIDDigestors(contentBytes, ctCode, haCode, mbChar)
    testEncodedCIDDigestors(contentBytes, ctName, haCode, mbChar)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haCode, mbChar)

    testEncodedCIDDigestors(cidContent, ctCodec, haName, mbChar)
    testEncodedCIDDigestors(cidContent, ctCode, haName, mbChar)
    testEncodedCIDDigestors(cidContent, ctName, haName, mbChar)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haName, mbChar)

    testEncodedCIDDigestors(contentBytes, ctCodec, haName, mbChar)
    testEncodedCIDDigestors(contentBytes, ctCode, haName, mbChar)
    testEncodedCIDDigestors(contentBytes, ctName, haName, mbChar)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haName, mbChar)

    testEncodedCIDDigestors(cidContent, ctCodec, haLabel, mbChar)
    testEncodedCIDDigestors(cidContent, ctCode, haLabel, mbChar)
    testEncodedCIDDigestors(cidContent, ctName, haLabel, mbChar)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haLabel, mbChar)

    testEncodedCIDDigestors(contentBytes, ctCodec, haLabel, mbChar)
    testEncodedCIDDigestors(contentBytes, ctCode, haLabel, mbChar)
    testEncodedCIDDigestors(contentBytes, ctName, haLabel, mbChar)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haLabel, mbChar)

    testEncodedCIDDigestors(cidContent, ctCodec, haToString, mbChar)
    testEncodedCIDDigestors(cidContent, ctCode, haToString, mbChar)
    testEncodedCIDDigestors(cidContent, ctName, haToString, mbChar)
    testEncodedCIDDigestors(cidContent, ctPrefixed, haToString, mbChar)

    testEncodedCIDDigestors(contentBytes, ctCodec, haToString, mbChar)
    testEncodedCIDDigestors(contentBytes, ctCode, haToString, mbChar)
    testEncodedCIDDigestors(contentBytes, ctName, haToString, mbChar)
    testEncodedCIDDigestors(contentBytes, ctPrefixed, haToString, mbChar)

  test(
    "CID digestors for content, content type, hash algorithm, and base encoding perform failure modes for invalid CIDs"
  ):
    val expected = cidBytes.toSeq

    def testEncodedCIDDigestorFailures[T, H, B](contentType: T, hash: H, base: B, msg: String)(using
        MulticodecIngest[T],
        MultihashFactory[H],
        MultibaseFactory[Array[Byte], B]
    ): Unit =
      assertEquals(CID.digestValidated(cidContent, contentType, hash, base), Left(msg))
      assertEquals(CID.digestIfValid(cidContent, contentType, hash, base), None)
      interceptMessage[CIDValidationError](msg):
        CID.digest(cidContent, contentType, hash, base)

    val badMC: VarInt = VarInt.encode(0x05)

    testEncodedCIDDigestorFailures(
      badMC,
      cidMultihashAlgorithm,
      cidMultibaseAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testEncodedCIDDigestorFailures(
      "invalid",
      cidMultihashAlgorithm,
      cidMultibaseAlgorithm,
      "Invalid multicodec name: 'invalid'"
    )
    testEncodedCIDDigestorFailures(
      badMC.toBytes :+ 85.toByte,
      cidMultihashAlgorithm,
      cidMultibaseAlgorithm,
      "Invalid multicodec code: '0x05'"
    )
    testEncodedCIDDigestorFailures(
      Array(-127.toByte, -14.toByte),
      cidMultihashAlgorithm,
      cidMultibaseAlgorithm,
      "Could not extract a varint from bytes"
    )
    testEncodedCIDDigestorFailures(
      cidContentType,
      VarInt.encode(0x04),
      cidMultibaseAlgorithm,
      "Unsupported multihash code: '0x04'"
    )
    testEncodedCIDDigestorFailures(
      cidContentType,
      "invalid",
      cidMultibaseAlgorithm,
      "Unsupported multihash name: 'invalid'"
    )
    testEncodedCIDDigestorFailures(
      cidContentType,
      cidMultihashAlgorithm,
      "~",
      "Unsupported multibase prefix character: '~'"
    )
    testEncodedCIDDigestorFailures(
      cidContentType,
      cidMultihashAlgorithm,
      "invalid",
      "Unsupported multibase codec name: 'invalid'"
    )

  test("CID[?].toBytes returns the bytes representing the full CID"):
    assertEquals(CID(cidBytes).toBytes.toSeq, cidBytes.toSeq)
    assertEquals(CID(cidMultibase).toBytes.toSeq, cidMultibase.toBytes.toSeq)

  test("CID[Encoded].toMultibase returns the Multibase objects representing the full encoded CID"):
    assertEquals(CID(cidMultibase).toMultibase, cidMultibase)

  test("CID[Raw].encode(base) converts a CID[Raw] to a CID[Encoded] using the algorithm 'base'"):
    assertEquals(CID(cidBytes).encode(cidMultibaseAlgorithm), CID(cidMultibase))

  test("CID[Encoded].decode converts a CID[Encoded] to a CID[Raw]"):
    assertEquals(CID(cidMultibase).decode.toBytes.toSeq, CID(cidBytes).toBytes.toSeq)

  test("CID congruency (=~, !~) acts as equality between maximal shared data of two CIDs"):
    val (cidRaw1, cidEncoded1) = (CID(cidContentType, cidAddress), CID(cidMultibase))
    val (cidRaw2, cidEncoded2) =
      (CID(cidBytes), CID(cidContentType, cidAddress, cidMultibaseAlgorithm))
    assert(cidRaw1 =~ cidRaw2)
    assert(cidRaw1 =~ cidEncoded2)
    assert(cidEncoded1 =~ cidRaw2)
    assert(cidEncoded1 =~ cidEncoded2)

    val (cidRaw3, cidEncoded3) =
      (CID(cidBytes.init :+ 40.toByte), CID(Multibase(cidMultibase.toString.init + "5")))
    assert(cidRaw1 !~ cidRaw3)
    assert(cidRaw1 !~ cidEncoded3)
    assert(cidEncoded1 !~ cidRaw3)
    assert(cidEncoded1 !~ cidEncoded3)

  test("CID congruency (=~, !~) does not compile for other types (e.g. Array[Byte] or Multibase)"):
    assertNoDiff(
      compileErrors("""CID(Array[Byte](1, 85, 22, 1, -52)) =~ Array[Byte](1, 85, 22, 1, -52)"""),
      """|error:
         |value =~ is not a member of multiformats.cid.CID[multiformats.cid.Raw].
         |An extension method was tried, but could not be fully constructed:
         |
         |    multiformats.cid.CID.=~()
         |CID(Array[Byte](1, 85, 22, 1, -52)) =~ Array[Byte](1, 85, 22, 1, -52)
         |                                   ^
         |""".stripMargin
    )
    assertNoDiff(
      compileErrors("""CID(Multibase("z9igAq1")) !~ Multibase("z9igAq1")"""),
      """|error:
         |value !~ is not a member of multiformats.cid.CID[multiformats.cid.Encoded].
         |An extension method was tried, but could not be fully constructed:
         |
         |    multiformats.cid.CID.!~()
         |CID(Multibase("z9igAq1")) !~ Multibase("z9igAq1")
         |                         ^
         |""".stripMargin
    )

  test("CID[?].codec returns the Multicodec representing the CID version"):
    assertEquals(CID(cidBytes).codec, cidVersion)
    assertEquals(CID(cidMultibase).codec, cidVersion)

  test("CID[?].contentType returns the Multicodec representing the CID's content type"):
    assertEquals(CID(cidBytes).contentType, cidContentType)
    assertEquals(CID(cidMultibase).contentType, cidContentType)

  test("CID[?].address returns the Multihash of the content this CID addresses"):
    assertEquals(CID(cidBytes).contentType, cidContentType)
    assertEquals(CID(cidMultibase).contentType, cidContentType)

  test("CID[Encoded].encoding returns the MultibaseAlgorithm encoding this CID"):
    assertEquals(CID(cidMultibase).encoding, cidMultibaseAlgorithm)

  test("CID[Encoded].toHumanReadable returns the human-readable format of this CID"):
    assertEquals(CID(cidMultibase).toHumanReadable, cidHumanReadable)

  test("CID[?].isAddressOf checks if this CID addresses the provided content"):
    assertEquals(CID(cidBytes).isAddressOf(cidContent), true)
    assertEquals(CID(cidBytes).isAddressOf(cidContent + "!"), false)
    assertEquals(CID(cidMultibase).isAddressOf(cidContent), true)
    assertEquals(CID(cidMultibase).isAddressOf("Now " + cidContent), false)

  test("String context prefix 'ci' on a multibase-encoded CID returns a CID[Encoded] object"):
    assertEquals(
      ci"base32z - cidv1 - raw - $mhHumanReadable",
      CID(s"base32z - cidv1 - raw - $mhHumanReadable")
    )
    interceptMessage[CIDValidationError]("Unsupported multibase codec name: 'base137'"):
      ci"base137 - cidv1 - raw - $mhHumanReadable"
