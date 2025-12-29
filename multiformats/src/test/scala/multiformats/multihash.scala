package multiformats.multihash

import multiformats.varint.VarInt

class MultihashTests extends munit.FunSuite:
  private val source: Array[Byte] = "foo".getBytes

  private def byteArray(bytes: Int*): Array[Byte] = Array(bytes*).map(_.toByte)

  // multihash -> (code as bytes, size as byte, digest as bytes, MultihashAlgorithm, human-readable)
  // All generated with other crypto libraries from the source string above
  private val validMultihashes
      : Map[Array[Byte], (Array[Byte], Byte, Array[Byte], MultihashAlgorithm, String)] =
    Vector(
      (
        MultihashAlgorithm.md4,
        byteArray(-44, 1),
        16.toByte,
        byteArray(10, -58, 112, 12, 73, 29, 112, -5, -122, 80, -108, 11, 28, -95, -28, -78),
        "md4-128-0ac6700c491d70fb8650940b1ca1e4b2"
      ),
      (
        MultihashAlgorithm.md5,
        byteArray(-43, 1),
        16.toByte,
        byteArray(-84, -67, 24, -37, 76, -62, -8, 92, -19, -17, 101, 79, -52, -60, -92, -40),
        "md5-128-acbd18db4cc2f85cedef654fccc4a4d8"
      ),
      (
        MultihashAlgorithm.sha1,
        byteArray(17),
        20.toByte,
        byteArray(11, -18, -57, -75, -22, 63, 15, -37, -55, 93, 13, -44, 127, 60, 91, -62, 117, -38,
          -118, 51),
        "sha1-160-0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
      ),
      (
        MultihashAlgorithm.sha2_224,
        byteArray(-109, 32),
        28.toByte,
        byteArray(8, 8, -10, 78, 96, -43, -119, 121, -4, -74, 118, -55, 110, -55, 56, 39, 13, -22,
          66, 68, 90, -18, -4, -45, -92, -26, -8, -37),
        "sha2-224-224-0808f64e60d58979fcb676c96ec938270dea42445aeefcd3a4e6f8db"
      ),
      (
        MultihashAlgorithm.sha2_256,
        byteArray(18),
        32.toByte,
        byteArray(44, 38, -76, 107, 104, -1, -58, -113, -7, -101, 69, 60, 29, 48, 65, 52, 19, 66,
          45, 112, 100, -125, -65, -96, -7, -118, 94, -120, 98, 102, -25, -82),
        "sha2-256-256-2c26b46b68ffc68ff99b453c1d30413413422d706483bfa0f98a5e886266e7ae"
      ),
      (
        MultihashAlgorithm.sha2_384,
        byteArray(32),
        48.toByte,
        byteArray(
          -104, -63, 31, -3, -3, -43, 64, 103, 107, 26, 19, 124, -79, -94, 43, 42, 112, 53, 12,
          -102, 68, 23, 29, 107, 17, -128, -58, -66, 92, -69, 46, -29, -9, -99, 83, 44, -118, 29,
          -39, -17, 46, -114, 8, -25, 82, -93, -70, -69
        ),
        "sha2-384-384-98c11ffdfdd540676b1a137cb1a22b2a70350c9a44171d6b1180c6be5cbb2ee3f79d532c8a1dd9ef2e8e08e752a3babb"
      ),
      (
        MultihashAlgorithm.sha2_512,
        byteArray(19),
        64.toByte,
        byteArray(
          -9, -5, -70, 110, 6, 54, -8, -112, -27, 111, -69, -13, 40, 62, 82, 76, 111, -93, 32, 74,
          -30, -104, 56, 45, 98, 71, 65, -48, -36, 102, 56, 50, 110, 40, 44, 65, -66, 94, 66, 84,
          -40, -126, 7, 114, -59, 81, -118, 44, 90, -116, 12, 127, 126, -38, 25, 89, 74, 126, -75,
          57, 69, 62, 30, -41
        ),
        "sha2-512-512-f7fbba6e0636f890e56fbbf3283e524c6fa3204ae298382d624741d0dc6638326e282c41be5e4254d8820772c5518a2c5a8c0c7f7eda19594a7eb539453e1ed7"
      ),
      (
        MultihashAlgorithm.sha2_512_224,
        byteArray(-108, 32),
        28.toByte,
        byteArray(-42, -113, 37, -115, 55, -42, 112, -49, -63, -20, 16, 1, -96, 57, 71, -124, 35,
          63, -120, -16, 86, -103, 79, -102, 126, 94, -103, -66),
        "sha2-512-224-224-d68f258d37d670cfc1ec1001a0394784233f88f056994f9a7e5e99be"
      ),
      (
        MultihashAlgorithm.sha2_512_256,
        byteArray(-107, 32),
        32.toByte,
        byteArray(-43, -128, 66, -26, -86, 90, 51, 94, 3, -83, 87, 108, 106, -98, 67, -76, 21, -111,
          -65, -46, 7, 127, 114, -34, -55, -33, 121, 48, -28, -110, 5, 93),
        "sha2-512-256-256-d58042e6aa5a335e03ad576c6a9e43b41591bfd2077f72dec9df7930e492055d"
      ),
      (
        MultihashAlgorithm.sha3_224,
        byteArray(23),
        28.toByte,
        byteArray(-12, -10, 119, -98, 21, 60, 57, 27, -67, 41, -55, 94, 114, -80, 112, -114, 57,
          -39, 22, 108, 124, -22, 81, -47, -15, 14, -11, -118),
        "sha3-224-224-f4f6779e153c391bbd29c95e72b0708e39d9166c7cea51d1f10ef58a"
      ),
      (
        MultihashAlgorithm.sha3_256,
        byteArray(22),
        32.toByte,
        byteArray(118, -45, -68, 65, -55, -11, -120, -9, -4, -48, -43, -65, 71, 24, -8, -8, 75, 28,
          65, -78, 8, -126, 112, 49, 0, -71, -21, -108, 19, -128, 124, 1),
        "sha3-256-256-76d3bc41c9f588f7fcd0d5bf4718f8f84b1c41b20882703100b9eb9413807c01"
      ),
      (
        MultihashAlgorithm.sha3_384,
        byteArray(21),
        48.toByte,
        byteArray(
          102, 85, 81, -110, -115, 19, -73, -40, 78, -32, 39, 52, 80, 43, 1, -115, -119, 106, 15,
          -72, 126, -19, 90, -37, 76, -121, -70, -111, -69, -42, 72, -108, 16, -31, 27, 15, -68,
          -64, 110, -41, -48, -21, -83, 85, -98, 93, 59, -75
        ),
        "sha3-384-384-665551928d13b7d84ee02734502b018d896a0fb87eed5adb4c87ba91bbd6489410e11b0fbcc06ed7d0ebad559e5d3bb5"
      ),
      (
        MultihashAlgorithm.sha3_512,
        byteArray(20),
        64.toByte,
        byteArray(
          75, -54, 43, 19, 126, -36, 88, 15, -27, 10, -120, -104, 62, -8, 96, -21, -84, -93, 108,
          -123, 123, 31, 73, 40, 57, -42, -41, 57, 36, 82, -90, 60, -126, -53, -21, -58, -114, 59,
          112, -94, -95, 72, 11, 75, -75, -44, 55, -89, -53, -90, -20, -7, -40, -97, -97, -13, -52,
          -47, 76, -42, 20, 110, -89, -25
        ),
        "sha3-512-512-4bca2b137edc580fe50a88983ef860ebaca36c857b1f492839d6d7392452a63c82cbebc68e3b70a2a1480b4bb5d437a7cba6ecf9d89f9ff3ccd14cd6146ea7e7"
      ),
      (
        MultihashAlgorithm.keccak_224,
        byteArray(26),
        28.toByte,
        byteArray(-38, -87, 77, -89, -10, -128, 107, -11, -92, -32, -81, 96, 55, -99, 117, -58, 44,
          -83, -42, -66, 84, 39, -63, 109, 1, -25, 108, -54),
        "keccak-224-224-daa94da7f6806bf5a4e0af60379d75c62cadd6be5427c16d01e76cca"
      ),
      (
        MultihashAlgorithm.keccak_256,
        byteArray(27),
        32.toByte,
        byteArray(65, -79, -96, 100, -105, 82, -81, 27, 40, -77, -36, 41, -95, 85, 110, -18, 120,
          30, 74, 76, 58, 31, -129, 83, -7, 15, -88, 52, -34, 9, -116, 77),
        "keccak-256-256-41b1a0649752af1b28b3dc29a1556eee781e4a4c3a1f7f53f90fa834de098c4d"
      ),
      (
        MultihashAlgorithm.keccak_384,
        byteArray(28),
        48.toByte,
        byteArray(
          25, -45, -8, 96, 125, 44, 101, 25, 68, 58, -73, 11, -15, -9, -56, 110, -99, -92, -3, -89,
          -5, -53, -89, -65, -82, 12, -85, 97, -112, -46, 70, 6, -12, -125, 52, -89, 56, 44, 96,
          -37, 71, -99, 73, -65, -39, -6, -127, 92
        ),
        "keccak-384-384-19d3f8607d2c6519443ab70bf1f7c86e9da4fda7fbcba7bfae0cab6190d24606f48334a7382c60db479d49bfd9fa815c"
      ),
      (
        MultihashAlgorithm.keccak_512,
        byteArray(29),
        64.toByte,
        byteArray(
          21, -105, -124, 42, -84, 82, -68, -99, 19, -2, 36, -99, -128, -118, -5, -12, 77, -95, 53,
          36, 117, -108, 119, 64, 76, 53, -110, -18, 51, 17, 115, -24, -97, -31, -53, -14, 26, 126,
          67, 96, -103, 13, 86, 95, -83, 70, 67, -51, -78, 9, -40, 15, -92, 26, -111, -34, -87, 126,
          102, 80, 34, -55, 33, 53
        ),
        "keccak-512-512-1597842aac52bc9d13fe249d808afbf44da13524759477404c3592ee331173e89fe1cbf21a7e4360990d565fad4643cdb209d80fa41a91dea97e665022c92135"
      ),
      (
        MultihashAlgorithm.blake2b_256,
        byteArray(-96, -28, 2),
        32.toByte,
        byteArray(-72, -2, -97, -129, 98, 85, -90, -6, 8, -10, 104, -85, 99, 42, -115, 8, 26, -40,
          121, -125, -57, 124, -46, 116, -28, -116, -28, 80, -16, -77, 73, -3),
        "blake2b-256-256-b8fe9f7f6255a6fa08f668ab632a8d081ad87983c77cd274e48ce450f0b349fd"
      ),
      (
        MultihashAlgorithm.blake2b_512,
        byteArray(-64, -28, 2),
        64.toByte,
        byteArray(
          -54, 0, 35, 48, -26, -99, 62, 107, -124, -92, 106, 86, -90, 83, 63, -41, -99, 81, -39,
          122, 59, -73, -54, -42, -62, -1, 67, -77, 84, 24, 93, 109, -63, -25, 35, -5, 61, -76, -82,
          7, 55, -31, 32, 55, -124, 36, -57, 20, -69, -104, 45, -99, -59, -69, -41, -96, -85, 49,
          -126, 64, -35, -47, -113, -115
        ),
        "blake2b-512-512-ca002330e69d3e6b84a46a56a6533fd79d51d97a3bb7cad6c2ff43b354185d6dc1e723fb3db4ae0737e120378424c714bb982d9dc5bbd7a0ab318240ddd18f8d"
      ),
      (
        MultihashAlgorithm.blake2s_128,
        byteArray(-48, -28, 2),
        16.toByte,
        byteArray(68, 71, -46, 9, 33, -17, -28, 16, 60, 86, -90, -107, -36, -86, -6, 56),
        "blake2s-128-128-4447d20921efe4103c56a695dcaafa38"
      ),
      (
        MultihashAlgorithm.blake2s_256,
        byteArray(-32, -28, 2),
        32.toByte,
        byteArray(8, -42, -54, -40, -128, 117, -34, -113, 25, 45, -80, -105, 87, 61, 14, -126, -108,
          17, -51, -111, -21, 110, -58, 94, -113, -63, 108, 1, 126, -33, -37, 116),
        "blake2s-256-256-08d6cad88075de8f192db097573d0e829411cd91eb6ec65e8fc16c017edfdb74"
      ),
      (
        MultihashAlgorithm.blake3,
        byteArray(30),
        32.toByte,
        byteArray(4, -32, -69, 57, -13, 11, 26, 63, -21, -119, -11, 54, -55, 59, -31, 80, 85, 72,
          45, -9, 72, 103, 75, 0, -46, 110, 90, 117, 119, 119, 2, -23),
        "blake3-256-04e0bb39f30b1a3feb89f536c93be15055482df748674b00d26e5a75777702e9"
      )
    ).map { case (ha, code, size, digest, hrString) =>
      ((code :+ size.toByte) ++ digest) -> (code, size, digest, ha, hrString)
    }.toMap

  private val invalidMultihashes: Map[(MultihashAlgorithm, Array[Byte]), String] = Map(
    (
      MultihashAlgorithm.sha1,
      byteArray(17, 20, 11, -18, -57, -75, -22, 63, 15, -37, -55, 93)
    ) -> "Mismatch between expected and realized digest sizes: 20 vs 10",
    (
      MultihashAlgorithm.sha1,
      byteArray(17, -11, -63, -15)
    ) -> "Invalid multihash format: could not extract code & size varints"
  )
  private val invalidCodeAndDigest: Map[(VarInt, Array[Byte]), String] = Map(
    (
      VarInt(0x04),
      byteArray(
        11, -18, -57, -75, -22, 63, 15, -37, -55, 93, 13, -44, 127, 60, 91, -62, 117, -38, -118, 51
      )
    ) -> "Unsupported multihash code: '0x04'"
  )
  private val invalidNameAndDigest: Map[(String, Array[Byte], String), String] = Map(
    (
      "hash-one",
      byteArray(
        11, -18, -57, -75, -22, 63, 15, -37, -55, 93, 13, -44, 127, 60, 91, -62, 117, -38, -118, 51
      ),
      "hash-one-160-0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
    ) -> "Unsupported multihash name: 'hash-one'"
  )

  test("Multihash.validated(Array[Byte]) returns Right[Multihash] for valid bytes"):
    validMultihashes.keys.foreach { value =>
      assertEquals(Multihash.validated(value).map(_.toBytes.toSeq), Right(value.toSeq))
    }

  test("Multihash.validated(Array[Byte]) returns Left[String] for invalid bytes"):
    invalidMultihashes.foreach { case ((_, value), msg) =>
      assertEquals(Multihash.validated(value).map(_.toBytes.toSeq), Left(msg))
    }

  test("Multihash.ifValid(Array[Byte]) returns Some[Multihash] for valid bytes"):
    validMultihashes.keys.foreach { value =>
      assertEquals(Multihash.ifValid(value).map(_.toBytes.toSeq), Some(value.toSeq))
    }

  test("Multihash.ifValid(Array[Byte]) returns None for invalid bytes"):
    invalidMultihashes.foreach { case ((_, value), _) =>
      assertEquals(Multihash.ifValid(value).map(_.toBytes.toSeq), None)
    }

  test("Multihash(Array[Byte]) returns Multihash for valid bytes"):
    validMultihashes.keys.foreach { value =>
      assertEquals(Multihash(value).toBytes.toSeq, value.toSeq)
    }

  test("Multihash(Array[Byte]) throws for invalid bytes"):
    invalidMultihashes.foreach { case ((_, value), msg) =>
      interceptMessage[MultihashValidationError](msg):
        Multihash(value)
    }

  test(
    "Multihash.validated(Array[Byte], MultihashAlgorithm|VarInt|String) returns Right[Multihash] for a supported algorithm"
  ):
    validMultihashes.foreach { case (value, (_, _, digest, algo, _)) =>
      assertEquals(Multihash.validated(digest, algo).map(_.toBytes.toSeq), Right(value.toSeq))
      assertEquals(Multihash.validated(digest, algo.code).map(_.toBytes.toSeq), Right(value.toSeq))
      assertEquals(Multihash.validated(digest, algo.name).map(_.toBytes.toSeq), Right(value.toSeq))
    }

  test(
    "Multihash.validated(Array[Byte], MultihashAlgorithm|VarInt|String) returns Left[String] for an unsupported algorithm"
  ):
    invalidCodeAndDigest.foreach { case ((code, digest), msg) =>
      assertEquals(Multihash.validated(digest, code).map(_.toBytes.toSeq), Left(msg))
    }
    invalidNameAndDigest.foreach { case ((code, digest, _), msg) =>
      assertEquals(Multihash.validated(digest, code).map(_.toBytes.toSeq), Left(msg))
    }

  test(
    "Multihash.ifValid(Array[Byte], MultihashAlgorithm|VarInt|String) returns Some[Multihash] for a supported algorithm"
  ):
    validMultihashes.foreach { case (value, (_, _, digest, algo, _)) =>
      assertEquals(Multihash.ifValid(digest, algo).map(_.toBytes.toSeq), Some(value.toSeq))
      assertEquals(Multihash.ifValid(digest, algo.code).map(_.toBytes.toSeq), Some(value.toSeq))
      assertEquals(Multihash.ifValid(digest, algo.name).map(_.toBytes.toSeq), Some(value.toSeq))
    }

  test(
    "Multihash.ifValid(Array[Byte], MultihashAlgorithm|VarInt|String) returns None for an unsupported algorithm"
  ):
    invalidCodeAndDigest.foreach { case ((code, digest), _) =>
      assertEquals(Multihash.ifValid(digest, code).map(_.toBytes.toSeq), None)
    }
    invalidNameAndDigest.foreach { case ((code, digest, _), _) =>
      assertEquals(Multihash.ifValid(digest, code).map(_.toBytes.toSeq), None)
    }

  test(
    "Multihash(Array[Byte], MultihashAlgorithm|VarInt|String) returns Multihash for a supported algorithm"
  ):
    validMultihashes.foreach { case (value, (_, _, digest, algo, _)) =>
      assertEquals(Multihash(digest, algo).toBytes.toSeq, value.toSeq)
      assertEquals(Multihash(digest, algo.code).toBytes.toSeq, value.toSeq)
      assertEquals(Multihash(digest, algo.name).toBytes.toSeq, value.toSeq)
    }

  test(
    "Multihash(Array[Byte], MultihashAlgorithm|VarInt|String) throws for an unsupported algorithm"
  ):
    invalidCodeAndDigest.foreach { case ((code, digest), msg) =>
      interceptMessage[MultihashValidationError](msg):
        Multihash(digest, code)
    }
    invalidNameAndDigest.foreach { case ((code, digest, _), msg) =>
      interceptMessage[MultihashValidationError](msg):
        Multihash(digest, code)
    }

  test("Multihash.validated(String) returns Right[Multihash] for a valid human-readable string"):
    validMultihashes.foreach { case (value, (_, _, _, _, hrString)) =>
      assertEquals(Multihash.validated(hrString).map(_.toBytes.toSeq), Right(value.toSeq))
    }

  test("Multihash.validated(String) returns Left[String] for an invalid human-readable string"):
    invalidNameAndDigest.foreach { case ((_, _, hrString), msg) =>
      assertEquals(Multihash.validated(hrString).map(_.toBytes.toSeq), Left(msg))
    }

  test("Multihash.ifValid(String) returns Some[Multihash] for a valid human-readable string"):
    validMultihashes.foreach { case (value, (_, _, _, _, hrString)) =>
      assertEquals(Multihash.ifValid(hrString).map(_.toBytes.toSeq), Some(value.toSeq))
    }

  test("Multihash.ifValid(String) returns None for an invalid human-readable string"):
    invalidNameAndDigest.foreach { case ((_, _, hrString), _) =>
      assertEquals(Multihash.ifValid(hrString).map(_.toBytes.toSeq), None)
    }

  test("Multihash(String) returns Multihash for a valid human-readable string"):
    validMultihashes.foreach { case (value, (_, _, _, _, hrString)) =>
      assertEquals(Multihash(hrString).toBytes.toSeq, value.toSeq)
    }

  test("Multihash(String) throws for an invalid human-readable string"):
    invalidNameAndDigest.foreach { case ((_, _, hrString), msg) =>
      interceptMessage[MultihashValidationError](msg):
        Multihash(hrString)
    }

  test(
    "Multihash.digestValidated(Array[Byte], MultihashAlgorithm|VarInt|String) returns Right[Multihash] on success"
  ):
    validMultihashes.foreach { case (value, (_, _, _, algo, _)) =>
      assertEquals(Multihash.digestValidated(source, algo).map(_.toBytes.toSeq), Right(value.toSeq))
      assertEquals(
        Multihash.digestValidated(source, algo.code).map(_.toBytes.toSeq),
        Right(value.toSeq)
      )
      assertEquals(
        Multihash.digestValidated(source, algo.name).map(_.toBytes.toSeq),
        Right(value.toSeq)
      )
    }

  test(
    "Multihash.digestValidated(Array[Byte], MultihashAlgorithm|VarInt|String) returns Left[String] on failure"
  ):
    invalidCodeAndDigest.foreach { case ((code, _), msg) =>
      assertEquals(Multihash.digestValidated(source, code).map(_.toBytes.toSeq), Left(msg))
    }
    invalidNameAndDigest.foreach { case ((code, _, _), msg) =>
      assertEquals(Multihash.digestValidated(source, code).map(_.toBytes.toSeq), Left(msg))
    }

  test(
    "Multihash.digestIfValid(Array[Byte], MultihashAlgorithm|VarInt|String) returns Some[Multihash] on success"
  ):
    validMultihashes.foreach { case (value, (_, _, _, algo, _)) =>
      assertEquals(Multihash.digestIfValid(source, algo).map(_.toBytes.toSeq), Some(value.toSeq))
      assertEquals(
        Multihash.digestIfValid(source, algo.code).map(_.toBytes.toSeq),
        Some(value.toSeq)
      )
      assertEquals(
        Multihash.digestIfValid(source, algo.name).map(_.toBytes.toSeq),
        Some(value.toSeq)
      )
    }

  test(
    "Multihash.digestIfValid(Array[Byte], MultihashAlgorithm|VarInt|String) returns None on failure"
  ):
    invalidCodeAndDigest.foreach { case ((code, _), _) =>
      assertEquals(Multihash.digestIfValid(source, code).map(_.toBytes.toSeq), None)
    }
    invalidNameAndDigest.foreach { case ((code, _, _), _) =>
      assertEquals(Multihash.digestIfValid(source, code).map(_.toBytes.toSeq), None)
    }

  test(
    "Multihash.digest(Array[Byte], MultihashAlgorithm|VarInt|String) returns Multihash on success"
  ):
    validMultihashes.foreach { case (value, (_, _, _, algo, _)) =>
      assertEquals(Multihash.digest(source, algo).toBytes.toSeq, value.toSeq)
      assertEquals(Multihash.digest(source, algo.code).toBytes.toSeq, value.toSeq)
      assertEquals(Multihash.digest(source, algo.name).toBytes.toSeq, value.toSeq)
    }

  test("Multihash.digest(Array[Byte], MultihashAlgorithm|VarInt|String) throws on failure"):
    invalidCodeAndDigest.foreach { case ((code, _), msg) =>
      interceptMessage[MultihashValidationError](msg):
        Multihash.digest(source, code)
    }
    invalidNameAndDigest.foreach { case ((code, _, _), msg) =>
      interceptMessage[MultihashValidationError](msg):
        Multihash.digest(source, code)
    }

  test("Multihash congruency (=~, !~) is just typed (in)equality (via BigInt)"):
    validMultihashes.foreach { case (value, (_, _, digest, algo, _)) =>
      val mh = Multihash(value)
      assert(mh =~ Multihash(digest, algo))
      assert(mh !~ Multihash(digest :+ 1.toByte, algo))
    }

  test("Multihash congruency (=~, !~) does not compile for other types (including Array[Byte])"):
    assertNoDiff(
      compileErrors("""Multihash(Array[Byte](17, 1, 20)) =~ Array[Byte](17, 1, 20)"""),
      """|error:
         |Found:    Array[Byte]
         |Required: multiformats.multihash.Multihash
         |Multihash(Array[Byte](17, 1, 20)) =~ Array[Byte](17, 1, 20)
         |                                    ^
         |""".stripMargin
    )
    assertNoDiff(
      compileErrors("""Multihash(Array[Byte](17, 1, 20)) !~ Array[Byte](17, 1, 21)"""),
      """|error:
         |Found:    Array[Byte]
         |Required: multiformats.multihash.Multihash
         |Multihash(Array[Byte](17, 1, 20)) !~ Array[Byte](17, 1, 21)
         |                                    ^
         |""".stripMargin
    )

  test("multihash.toBytes returns the bytes representing the full multihash"):
    validMultihashes.keys.foreach { value =>
      assertEquals(Multihash(value).toBytes.toSeq, value.toSeq)
    }

  test("multihash.code returns the multicodec code from from the front of its structure"):
    validMultihashes.foreach { case (value, (code, _, _, _, _)) =>
      assertEquals(Multihash(value).code, VarInt(code))
    }

  test("multihash.algorithm returns the MultihashAlgorithm instance for its multicodec code"):
    validMultihashes.foreach { case (value, (_, _, _, algo, _)) =>
      assertEquals(Multihash(value).algorithm, algo)
    }

  test("multihash.size returns the number of digest bytes as represented second in its structure"):
    validMultihashes.foreach { case (value, (_, size, _, _, _)) =>
      assertEquals(Multihash(value).size, size.toInt)
    }

  test("multihash.digest returns the digest bytes with the varint prefixes removed"):
    validMultihashes.foreach { case (value, (_, _, digest, _, _)) =>
      assertEquals(Multihash(value).digest.toSeq, digest.toSeq)
    }

  test("multihash.toHumanReadable returns a human-readable string (base-16 encoded)"):
    validMultihashes.foreach { case (value, (_, _, _, _, hrString)) =>
      assertEquals(Multihash(value).toHumanReadable, hrString)
    }

  test("String context prefix 'mh' on a human-readable hash string returns a Multihash object"):
    assert(
      mh"md4-128-0ac6700c491d70fb8650940b1ca1e4b2" =~ Multihash(
        "md4-128-0ac6700c491d70fb8650940b1ca1e4b2"
      )
    )
    interceptMessage[MultihashValidationError]("Unsupported multihash name: 'hash-one'"):
      mh"hash-one-160-0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
