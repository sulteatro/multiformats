package multiformats.multibase

import milletre.constructor.ValidationError
import multiencoder.encoder.*
import multiencoder.encoding.Encoding

class MultibaseTests extends munit.FunSuite:
  private val source: String = "foo"

  private val validMultibases: Map[String, (MultibaseAlgorithm, Encoding[?])] = Map(
    "0011001100110111101101111" -> (MultibaseAlgorithm.base2, Base2),
    "731467557" -> (MultibaseAlgorithm.base8, Base8),
    "96713199" -> (MultibaseAlgorithm.base10, Base10),
    "f666f6f" -> (MultibaseAlgorithm.base16, Base16),
    "F666F6F" -> (MultibaseAlgorithm.base16upper, Base16Upper),
    "vcpnmu" -> (MultibaseAlgorithm.base32hex, Base32Hex),
    "VCPNMU" -> (MultibaseAlgorithm.base32hexupper, Base32HexUpper),
    "tcpnmu===" -> (MultibaseAlgorithm.base32hexpad, Base32HexPad),
    "TCPNMU===" -> (MultibaseAlgorithm.base32hexpadupper, Base32HexPadUpper),
    "bmzxw6" -> (MultibaseAlgorithm.base32, Base32),
    "BMZXW6" -> (MultibaseAlgorithm.base32upper, Base32Upper),
    "cmzxw6===" -> (MultibaseAlgorithm.base32pad, Base32Pad),
    "CMZXW6===" -> (MultibaseAlgorithm.base32padupper, Base32PadUpper),
    "hc3zs6" -> (MultibaseAlgorithm.base32z, Base32z),
    "k3zvxr" -> (MultibaseAlgorithm.base36, Base36),
    "K3ZVXR" -> (MultibaseAlgorithm.base36upper, Base36Upper),
    "zbQbp" -> (MultibaseAlgorithm.base58btc, Base58BTC),
    "ZApAP" -> (MultibaseAlgorithm.base58flickr, Base58Flickr),
    "mZm9v" -> (MultibaseAlgorithm.base64, Base64),
    "MZm9v" -> (MultibaseAlgorithm.base64pad, Base64Pad),
    "uZm9v" -> (MultibaseAlgorithm.base64url, Base64Url),
    "UZm9v" -> (MultibaseAlgorithm.base64urlpad, Base64UrlPad)
  )

  private val invalidPrefixes: Map[String, String] = Map(
    "1011001100" -> "Unsupported multibase prefix character: '1'",
    "*31467557" -> "Unsupported multibase prefix character: '*'",
    "RVD92EX0" -> "Multibase encoding not implemented: 'base45'"
  )

  private val invalidData: Map[String, String] = Map(
    "tcpnmu" -> "Missing expected right padding with '='",
    "t===cpnmu" -> "Found left padding with '=' instead of right padding",
    "vCPNMU" -> "Invalid encoding characters"
  )

  private val invalidMultibases: Map[String, String] = invalidPrefixes ++ invalidData

  test("Multibase.validated(String) returns Right[Multibase] for valid strings"):
    validMultibases.keys.foreach { value =>
      assertEquals(Multibase.validated(value).map(_.toString), Right(value))
    }

  test("Multibase.validated(String) returns Left[String] for invalid or unparseable strings"):
    invalidMultibases.foreach { case (value, msg) =>
      assertEquals(Multibase.validated(value).map(_.toString), Left(msg))
    }

  test("Multibase.ifValid(String) returns Some[Multibase] for valid strings"):
    validMultibases.keys.foreach { value =>
      assertEquals(Multibase.ifValid(value).map(_.toString), Some(value))
    }

  test("Multibase.ifValid(String) returns None for invalid or unparseable strings"):
    invalidMultibases.foreach { case (value, msg) =>
      assertEquals(Multibase.ifValid(value).map(_.toString), None)
    }

  test("Multibase(String) returns Multibase for valid strings"):
    validMultibases.keys.foreach { value =>
      assertEquals(Multibase(value).toString, value)
    }

  test("Multibase(String) throws for invalid or unparseable strings"):
    invalidMultibases.foreach { case (value, msg) =>
      interceptMessage[ValidationError[Multibase]](msg):
        Multibase(value)
    }

  test(
    "Multibase.validated(String|Array[Byte], MultibaseAlgorithm|String) returns Right[Multibase] for valid inputs"
  ):
    validMultibases
      .foreach { case (value, (code, _)) =>
        val (data, char, name) = (value.drop(1), code.character, code.toString)
        assertEquals(Multibase.validated(data.getBytes, code).map(_.toString), Right(value))
        assertEquals(Multibase.validated(data.getBytes, char).map(_.toString), Right(value))
        assertEquals(Multibase.validated(data.getBytes, name).map(_.toString), Right(value))
      }

  test(
    "Multibase.validated(String|Array[Byte], MultibaseAlgorithm|String) returns Left[String] for invalid inputs"
  ):
    invalidMultibases
      .flatMap { case (value, msg) =>
        MultibaseAlgorithm.getByChar(value.take(1)).toOption.map((value.drop(1), _, msg))
      }.foreach { case (value, code, msg) =>
        val (char, name) = (code.character, code.toString)
        assertEquals(Multibase.validated(value.getBytes, code).map(_.toString), Left(msg))
        assertEquals(Multibase.validated(value.getBytes, char).map(_.toString), Left(msg))
        assertEquals(Multibase.validated(value.getBytes, name).map(_.toString), Left(msg))
      }

  test(
    "Multibase.ifValid(String|Array[Byte], MultibaseAlgorithm|String) returns Some[Multibase] for valid inputs"
  ):
    validMultibases
      .foreach { case (value, (code, _)) =>
        val (data, char, name) = (value.drop(1), code.character, code.toString)
        assertEquals(Multibase.ifValid(data.getBytes, code).map(_.toString), Some(value))
        assertEquals(Multibase.ifValid(data.getBytes, char).map(_.toString), Some(value))
        assertEquals(Multibase.ifValid(data.getBytes, name).map(_.toString), Some(value))
      }

  test(
    "Multibase.ifValid(String|Array[Byte], MultibaseAlgorithm|String) returns None for invalid inputs"
  ):
    invalidMultibases
      .flatMap { case (value, msg) =>
        MultibaseAlgorithm.getByChar(value.take(1)).toOption.map((value.drop(1), _, msg))
      }.foreach { case (value, code, msg) =>
        val (char, name) = (code.character, code.toString)
        assertEquals(Multibase.ifValid(value.getBytes, code).map(_.toString), None)
        assertEquals(Multibase.ifValid(value.getBytes, char).map(_.toString), None)
        assertEquals(Multibase.ifValid(value.getBytes, name).map(_.toString), None)
      }

  test(
    "Multibase(String|Array[Byte], MultibaseAlgorithm|String) returns Multibase for valid inputs"
  ):
    validMultibases
      .foreach { case (value, (code, _)) =>
        val (data, char, name) = (value.drop(1), code.character, code.toString)
        assertEquals(Multibase(data.getBytes, code).toString, value)
        assertEquals(Multibase(data.getBytes, char).toString, value)
        assertEquals(Multibase(data.getBytes, name).toString, value)
      }

  test("Multibase(String|Array[Byte], MultibaseAlgorithm|String) throws for invalid inputs"):
    invalidMultibases
      .flatMap { case (value, msg) =>
        MultibaseAlgorithm.getByChar(value.take(1)).toOption.map((value.drop(1), _, msg))
      }.foreach { case (value, code, msg) =>
        val (char, name) = (code.character, code.toString)
        interceptMessage[ValidationError[Multibase]](msg):
          Multibase(value.getBytes, code).toString
        interceptMessage[ValidationError[Multibase]](msg):
          Multibase(value.getBytes, char).toString
        interceptMessage[ValidationError[Multibase]](msg):
          Multibase(value.getBytes, name).toString
      }

  test(
    "Multibase.encodeValidated(String|Array[Byte], MultibaseAlgorithm|String) returns Right[Multibase] on success"
  ):
    validMultibases.foreach { case (value, (code, _)) =>
      val (char, name) = (code.character, code.toString)
      assertEquals(Multibase.encodeValidated(source.getBytes, code).map(_.toString), Right(value))
      assertEquals(Multibase.encodeValidated(source.getBytes, char).map(_.toString), Right(value))
      assertEquals(Multibase.encodeValidated(source.getBytes, name).map(_.toString), Right(value))
    }

  test(
    "Multibase.encodeValidated(String|Array[Byte], MultibaseAlgorithm|String) returns Left[String] on failure"
  ):
    invalidPrefixes
      .flatMap { case (value, msg) =>
        MultibaseAlgorithm.getByChar(value.take(1)).toOption.map((_, msg))
      }.foreach { case (code, msg) =>
        val (char, name) = (code.character, code.toString)
        assertEquals(Multibase.encodeValidated(source.getBytes, code).map(_.toString), Left(msg))
        assertEquals(Multibase.encodeValidated(source.getBytes, char).map(_.toString), Left(msg))
        assertEquals(Multibase.encodeValidated(source.getBytes, name).map(_.toString), Left(msg))
      }

  test(
    "Multibase.encodeIfValid(String|Array[Byte], MultibaseAlgorithm|String) returns Some[Multibase] on success"
  ):
    validMultibases.foreach { case (value, (code, _)) =>
      val (char, name) = (code.character, code.toString)
      assertEquals(Multibase.encodeIfValid(source.getBytes, code).map(_.toString), Some(value))
      assertEquals(Multibase.encodeIfValid(source.getBytes, char).map(_.toString), Some(value))
      assertEquals(Multibase.encodeIfValid(source.getBytes, name).map(_.toString), Some(value))
    }

  test(
    "Multibase.encodeIfValid(String|Array[Byte], MultibaseAlgorithm|String) returns None on failure"
  ):
    invalidPrefixes
      .flatMap { case (value, msg) =>
        MultibaseAlgorithm.getByChar(value.take(1)).toOption.map((_, msg))
      }.foreach { case (code, msg) =>
        val (char, name) = (code.character, code.toString)
        assertEquals(Multibase.encodeIfValid(source.getBytes, code).map(_.toString), None)
        assertEquals(Multibase.encodeIfValid(source.getBytes, char).map(_.toString), None)
        assertEquals(Multibase.encodeIfValid(source.getBytes, name).map(_.toString), None)
      }

  test(
    "Multibase.encode(String|Array[Byte], MultibaseAlgorithm|String) returns Multibase on success"
  ):
    validMultibases.foreach { case (value, (code, _)) =>
      val (char, name) = (code.character, code.toString)
      assertEquals(Multibase.encode(source.getBytes, code).toString, value)
      assertEquals(Multibase.encode(source.getBytes, char).toString, value)
      assertEquals(Multibase.encode(source.getBytes, name).toString, value)
    }

  test("Multibase.encode(String|Array[Byte], MultibaseAlgorithm|String) throws on failure"):
    invalidPrefixes
      .flatMap { case (value, msg) =>
        MultibaseAlgorithm.getByChar(value.take(1)).toOption.map((_, msg))
      }.foreach { case (code, msg) =>
        val (char, name) = (code.character, code.toString)
        interceptMessage[ValidationError[Multibase]](msg):
          Multibase.encode(source.getBytes, code).toString
        interceptMessage[ValidationError[Multibase]](msg):
          Multibase.encode(source.getBytes, char).toString
        interceptMessage[ValidationError[Multibase]](msg):
          Multibase.encode(source.getBytes, name).toString
      }

  test("Multibase congruency (=~, !~) is just typed (in)equality"):
    validMultibases.keys.foreach { value =>
      val mb = Multibase(value)
      assert(mb =~ Multibase(value.drop(1).getBytes, value.take(1)))
      assert(mb =~ Multibase.encode(source.getBytes, value.take(1)))
      assert(mb !~ Multibase.encode((source ++ "bar").getBytes, value.take(1)))
    }

  test("Multibase congruency (=~, !~) does not compile for other types (including String)"):
    assertNoDiff(
      compileErrors("""Multibase("UZm9v") =~ "UZm9v""""),
      """|error:
         |Found:    ("UZm9v" : String)
         |Required: multiformats.multibase.Multibase
         |Multibase("UZm9v") =~ "UZm9v"
         |                     ^
         |""".stripMargin
    )
    assertNoDiff(
      compileErrors("""Multibase("UZm9v") !~ "UZm9v""""),
      """|error:
         |Found:    ("UZm9v" : String)
         |Required: multiformats.multibase.Multibase
         |Multibase("UZm9v") !~ "UZm9v"
         |                     ^
         |""".stripMargin
    )

  test("Multibase.toBytes returns the bytes associated with the multibase string"):
    validMultibases.keys.foreach { value =>
      assertEquals(Multibase(value).toBytes.toSeq, value.getBytes.toSeq)
    }

  test("Multibase.prefix returns the prefixed character code"):
    validMultibases.foreach { case (value, (code, _)) =>
      assertEquals(Multibase(value).prefix, code.character)
    }

  test("Multibase.data returns the string-encoded data"):
    validMultibases.keys.foreach { value =>
      assertEquals(Multibase(value).data, value.drop(1))
    }

  test("Multibase.encoding returns the MultibaseAlgorithm for its character prefix"):
    validMultibases.foreach { case (value, (code, _)) =>
      assertEquals(Multibase(value).encoding, code)
    }

  test("Multibase.encoder returns the BaseN encoder associated with this instance"):
    validMultibases.foreach { case (value, (_, encoder)) =>
      assertEquals(Multibase(value).encoder, encoder)
    }

  test("Multibase.decode decodes the data string into the original byte array"):
    validMultibases.foreach { case (value, (_, encoder)) =>
      assertEquals(Multibase(value).decode.toSeq, source.getBytes.toSeq)
    }

  test("String context prefix 'mb' returns a Multibase object"):
    assertEquals(mb"bmzxw6", Multibase("bmzxw6"))
    interceptMessage[ValidationError[Multibase]]("Invalid encoding characters"):
      mb"vCPNMU"
