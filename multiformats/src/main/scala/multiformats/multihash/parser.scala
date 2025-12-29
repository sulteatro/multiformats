package multiformats.multihash

import multiencoder.encoder.Base16
import multiformats.multicodec.Multicodec
import multiformats.varint.VarInt
import org.bouncycastle.crypto.digests.Blake3Digest
import org.bouncycastle.jce.provider.BouncyCastleProvider

import java.security.MessageDigest
import java.security.MessageDigestSpi
import java.security.Provider
import java.security.Security

final case class MultihashValidationError(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object providers:
  class Blake3Provider extends Provider("Blake3Provider", "1.0.0", "BLAKE3 MessageDigest Provider"):
    private class Blake3DigestSpi extends MessageDigestSpi:
      private val digest: Blake3Digest = new Blake3Digest
      private val singleByte: Array[Byte] = Array(0.toByte)

      override protected def engineUpdate(input: Byte): Unit =
        singleByte(0) = input
        digest.update(singleByte, 0, 1);

      override protected def engineUpdate(input: Array[Byte], offset: Int, len: Int): Unit =
        digest.update(input, offset, len);

      override protected def engineDigest: Array[Byte] =
        val output = Array.fill[Byte](digest.getDigestSize())(0)
        digest.doFinal(output, 0)
        output

      override protected def engineReset: Unit = digest.reset

      override protected def engineGetDigestLength: Int = digest.getDigestSize

    put("MessageDigest.BLAKE3", classOf[Blake3DigestSpi].getName)
    put("MessageDigest.blake3", classOf[Blake3DigestSpi].getName) // allow lowercase alias

// Note: this one is manually implemented, and thus must be manually updated
enum MultihashAlgorithm(val multicodec: Multicodec, val name: String):
  case md4 extends MultihashAlgorithm(Multicodec.md4, "md4")
  case md5 extends MultihashAlgorithm(Multicodec.md5, "md5")
  case sha1 extends MultihashAlgorithm(Multicodec.sha1, "sha1")
  case sha2_224 extends MultihashAlgorithm(Multicodec.sha2_224, "sha-224")
  case sha2_256 extends MultihashAlgorithm(Multicodec.sha2_256, "sha-256")
  case sha2_384 extends MultihashAlgorithm(Multicodec.sha2_384, "sha-384")
  case sha2_512 extends MultihashAlgorithm(Multicodec.sha2_512, "sha-512")
  case sha2_512_224 extends MultihashAlgorithm(Multicodec.sha2_512_224, "sha-512/224")
  case sha2_512_256 extends MultihashAlgorithm(Multicodec.sha2_512_256, "sha-512/256")
  case sha3_224 extends MultihashAlgorithm(Multicodec.sha3_224, "sha3-224")
  case sha3_256 extends MultihashAlgorithm(Multicodec.sha3_256, "sha3-256")
  case sha3_384 extends MultihashAlgorithm(Multicodec.sha3_384, "sha3-384")
  case sha3_512 extends MultihashAlgorithm(Multicodec.sha3_512, "sha3-512")
  case keccak_224 extends MultihashAlgorithm(Multicodec.keccak_224, "keccak-224")
  case keccak_256 extends MultihashAlgorithm(Multicodec.keccak_256, "keccak-256")
  case keccak_384 extends MultihashAlgorithm(Multicodec.keccak_384, "keccak-384")
  case keccak_512 extends MultihashAlgorithm(Multicodec.keccak_512, "keccak-512")
  case blake2b_256 extends MultihashAlgorithm(Multicodec.blake2b_256, "blake2b-256")
  case blake2b_512 extends MultihashAlgorithm(Multicodec.blake2b_512, "blake2b-512")
  case blake2s_128 extends MultihashAlgorithm(Multicodec.blake2s_128, "blake2s-128")
  case blake2s_256 extends MultihashAlgorithm(Multicodec.blake2s_256, "blake2s-256")
  case blake3 extends MultihashAlgorithm(Multicodec.blake3, "blake3")

  def label: String = this.toString.replaceAll("_", "-")

  def size: Int = MessageDigest.getInstance(name).getDigestLength

  def code: VarInt = multicodec.code

  def digest(barr: Array[Byte]): Array[Byte] =
    val messageDigest = MessageDigest.getInstance(name)
    messageDigest.update(barr)
    messageDigest.digest()

object MultihashAlgorithm:
  private lazy val codeToAlgorithm: Map[VarInt, MultihashAlgorithm] =
    MultihashAlgorithm.values.map(a => a.code -> a).toMap

  def byCode(code: VarInt): Either[String, MultihashAlgorithm] =
    codeToAlgorithm.get(code).toRight(s"Unsupported multihash code: '${code.toHex}'")

  private lazy val nameToAlgorithm: Map[String, MultihashAlgorithm] =
    MultihashAlgorithm.values.map {
      code => Vector(code.toString -> code, code.name -> code, code.label -> code)
    }.flatten.toMap

  def byName(name: String): Either[String, MultihashAlgorithm] =
    nameToAlgorithm.get(name.toLowerCase).toRight(s"Unsupported multihash name: '$name'")

//
// Implementation details:
// * validateBytes checks that the provided byte array is a valid multihash
// * buildWithCodec concatenates the codec, size, and digest into a multihash
// * digestWithCodec generates a hash digest and creates a multihash from it
//
private def validateBytes(bytes: Array[Byte]): Either[String, Array[Byte]] =
  VarInt.sequence(bytes, 2) match
    case (Array(code, size), digest) =>
      Multicodec.validated(code)
        .map(hashCodec => MultihashAlgorithm.byCode(hashCodec.code))
        .joinRight
        .filterOrElse(
          _ => size.decode.toInt.equals(digest.length),
          s"Mismatch between expected and realized digest sizes: $size vs ${digest.length}"
        ).map(_ => bytes)
    case _ =>
      Left("Invalid multihash format: could not extract code & size varints")

private def buildWithCodec(bytes: Array[Byte], algo: MultihashAlgorithm): Array[Byte] =
  (algo.code.toBytes :+ bytes.length.toByte) ++ bytes

private def digestWithCodec(
    bytes: Array[Byte],
    algo: MultihashAlgorithm
): Either[String, Array[Byte]] =
  (algo.size, algo.digest(bytes)) match
    case (size, digest) if size.equals(digest.length) =>
      Right(buildWithCodec(digest, algo))
    case (size, digest) =>
      Left(s"Mismatch between expected and realized digest sizes: $size vs ${digest.length}")

private def translateMultihash(str: String): Either[String, Array[Byte]] =
  str.split("-").reverse.toVector match
    case digest +: size +: nameParts =>
      val sizeBytes: Int = size.toInt / 8
      Base16.validate(digest)
        .map(Base16.decode)
        .map(digestBytes =>
          MultihashAlgorithm.byName(nameParts.reverse.mkString("-"))
            .filterOrElse(
              _ => sizeBytes.equals(digestBytes.length),
              s"Mismatch between expected and realized digest sizes: $sizeBytes vs ${digestBytes.length}"
            ).map(buildWithCodec(digestBytes, _))
        ).joinRight
    case _ =>
      Left("Invalid human-readable Multihash format: '$str'")

//
// Type-variadic construction via typeclass
//
sealed trait MultihashFactory[C]:
  def toMultihashAlgorithm(value: C): Either[String, MultihashAlgorithm]

  def build(value: Array[Byte], code: C): Either[String, Array[Byte]] =
    toMultihashAlgorithm(code).map(algo => buildWithCodec(value, algo))

  def digest(value: Array[Byte], code: C): Either[String, Array[Byte]] =
    toMultihashAlgorithm(code).map(algo => digestWithCodec(value, algo)).joinRight

private[multiformats] object MultihashFactory:
  given MultihashFactory[MultihashAlgorithm] =
    new MultihashFactory[MultihashAlgorithm]:
      def toMultihashAlgorithm(value: MultihashAlgorithm): Either[String, MultihashAlgorithm] =
        Right(value)

  given MultihashFactory[VarInt] =
    new MultihashFactory[VarInt]:
      def toMultihashAlgorithm(value: VarInt): Either[String, MultihashAlgorithm] =
        MultihashAlgorithm.byCode(value)

  given MultihashFactory[String] =
    new MultihashFactory[String]:
      def toMultihashAlgorithm(value: String): Either[String, MultihashAlgorithm] =
        MultihashAlgorithm.byName(value)

//
// Public object interface: Multihash
//
opaque type Multihash = Array[Byte]

object Multihash:
  // Add extra crypto algorithms provided by Bouncy Castle
  Security.addProvider(new BouncyCastleProvider)
  // Add custom Blake3Provider using Bouncy Castle's raw Blake3Digest algorithm
  Security.addProvider(new providers.Blake3Provider)

  //
  // Constructors that validate and type an existing multihash
  //
  def validated(bytes: Array[Byte]): Either[String, Multihash] = validateBytes(bytes)
  def ifValid(bytes: Array[Byte]): Option[Multihash] = validated(bytes).toOption
  def apply(bytes: Array[Byte]): Multihash =
    validated(bytes).fold(error => throw MultihashValidationError(error), identity)

  //
  // Constructors that build a multihash from a digest and multicodec
  //
  def validated[C](bytes: Array[Byte], code: C)(using
      c: MultihashFactory[C]
  ): Either[String, Multihash] = c.build(bytes, code)
  def ifValid[C](bytes: Array[Byte], code: C)(using c: MultihashFactory[C]): Option[Multihash] =
    c.build(bytes, code).toOption
  def apply[C](bytes: Array[Byte], code: C)(using c: MultihashFactory[C]): Multihash =
    c.build(bytes, code).fold(error => throw MultihashValidationError(error), identity)

  //
  // Constructors that translate a human-readable string into a Multihash
  //   Format: <multihash_algorithm_name>-<base16_encoded_hash>
  //
  def validated(str: String)(using c: MultihashFactory[String]): Either[String, Multihash] =
    translateMultihash(str)
  def ifValid(str: String)(using c: MultihashFactory[String]): Option[Multihash] =
    translateMultihash(str).toOption
  def apply(str: String)(using c: MultihashFactory[String]): Multihash =
    translateMultihash(str).fold(error => throw MultihashValidationError(error), identity)

  //
  // Hash-generating constructors
  //
  def digestValidated[C](bytes: Array[Byte], code: C)(using
      c: MultihashFactory[C]
  ): Either[String, Multihash] = c.digest(bytes, code)
  def digestIfValid[C](bytes: Array[Byte], code: C)(using
      c: MultihashFactory[C]
  ): Option[Multihash] = c.digest(bytes, code).toOption
  def digest[C](bytes: Array[Byte], code: C)(using c: MultihashFactory[C]): Multihash =
    c.digest(bytes, code).fold(error => throw MultihashValidationError(error), identity)

  extension (mh: Multihash)
    def =~(other: Multihash): Boolean = BigInt(mh).equals(BigInt(other))
    def !~(other: Multihash): Boolean = !(mh =~ other)

    def toBytes: Array[Byte] = mh

    def code: VarInt = VarInt.sequence(mh, 1).head.head
    def algorithm: MultihashAlgorithm =
      summon[MultihashFactory[VarInt]].toMultihashAlgorithm(code).toOption.get

    def size: Int = VarInt.sequence(mh, 2).head.last.decode.toInt
    def digest: Array[Byte] = VarInt.sequence(mh, 2).last

    def toHumanReadable: String =
      VarInt.sequence(mh, 2) match
        case (Array(mhc, mhs), mhd) =>
          Vector(
            MultihashAlgorithm.byCode(mhc).map(_.label).toOption.get,
            mhs.decode * 8,
            new String(Base16.encode(mhd))
          ).mkString("-")
        case (ba, via) =>
          throw MultihashValidationError(s"Unreachable case, bytes: ${ba}, varints: ${via}")

extension (sc: StringContext)
  def mh(args: Any*): Multihash = Multihash(sc.s(args*))
