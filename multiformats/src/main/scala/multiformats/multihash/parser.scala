package multiformats.multihash

import milletre.constructor.*
import multiencoder.encoder.Base16
import multiformats.multicodec.Multicodec
import multiformats.varint.VarInt

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
  def code: VarInt = multicodec.code
  def size: Int = security.size(name)
  def digest(barr: Array[Byte]): Either[String, Array[Byte]] = security.digest(name, barr)

object MultihashAlgorithm:
  private lazy val codeToAlgorithm: Map[VarInt, MultihashAlgorithm] =
    MultihashAlgorithm.values.map(a => a.code -> a).toMap

  private lazy val nameToAlgorithm: Map[String, MultihashAlgorithm] =
    MultihashAlgorithm.values.map {
      code => Vector(code.toString -> code, code.name -> code, code.label -> code)
    }.flatten.toMap

  def getByCode(code: VarInt): Either[String, MultihashAlgorithm] =
    codeToAlgorithm.get(code).toRight(s"Unsupported multihash code: '${code.toHex}'")

  def getByName(name: String): Either[String, MultihashAlgorithm] =
    nameToAlgorithm.get(name.toLowerCase).toRight(s"Unsupported multihash name: '$name'")

//
// Typeclass to get MultihashAlgorithm values by their properties
//
private[multiformats] trait MultihashAlgorithmFactory[C]
    extends EitherConversion[C, MultihashAlgorithm]
private[multiformats] object MultihashAlgorithmFactory:
  given MultihashAlgorithmFactory[MultihashAlgorithm] = v => Right(v)
  given MultihashAlgorithmFactory[VarInt] = v => MultihashAlgorithm.getByCode(v)
  given MultihashAlgorithmFactory[String] = v => MultihashAlgorithm.getByName(v)

private def buildWithCodec(bytes: Array[Byte], algo: MultihashAlgorithm): Array[Byte] =
  (algo.code.toBytes :+ bytes.length.toByte) ++ bytes

//
// Input validators: MultihashIngest
//
trait MultihashIngest[V] extends EitherConversion[V, Array[Byte]]

object MultihashIngest:

  // checks that the provided byte array is a valid multihash
  given MultihashIngest[Array[Byte]] =
    bytes =>
      VarInt.sequence(bytes, 2) match
        case (Array(code, size), digest) =>
          Multicodec.validated(code)
            .flatMap(hashCodec => MultihashAlgorithm.getByCode(hashCodec.code))
            .filterOrElse(
              _ => size.decode.toInt.equals(digest.length),
              s"Mismatch between expected and realized digest sizes: $size vs ${digest.length}"
            ).map(_ => bytes)
        case _ =>
          Left("Invalid multihash format: could not extract code & size varints")

  given [A] => (af: MultihashAlgorithmFactory[A]) => MultihashIngest[(Array[Byte], A)] =
    (bytes, algorithm) => af(algorithm).map(buildWithCodec(bytes, _))

  // checks that a string is in human-readable multhash format, then converts it to a multihash
  given MultihashIngest[String] =
    str =>
      str.split("-").reverse.toVector match
        case digest +: size +: nameParts =>
          val sizeBytes: Int = size.toInt / 8
          Base16.validate(digest)
            .map(Base16.decode)
            .map(digestBytes =>
              MultihashAlgorithm.getByName(nameParts.reverse.mkString("-"))
                .filterOrElse(
                  _ => sizeBytes.equals(digestBytes.length),
                  s"Mismatch between expected and realized digest sizes: $sizeBytes vs ${digestBytes.length}"
                ).map(buildWithCodec(digestBytes, _))
            ).joinRight
        case _ =>
          Left("Invalid human-readable Multihash format: '$str'")

//
// Public object interface: Multihash
//
opaque type Multihash = Array[Byte]

object Multihash extends MultiConstructor[Array[Byte], Multihash, MultihashIngest]:
  security.addMultihashProviders()

  //
  // Hash-generating constructors
  //
  def digestValidated[A](bytes: Array[Byte], algorithm: A)(using
      maf: MultihashAlgorithmFactory[A]
  ): Either[String, Multihash] =
    maf(algorithm).flatMap(algo => algo.digest(bytes).map(buildWithCodec(_, algo)))
  def digestIfValid[A](bytes: Array[Byte], algorithm: A)(using
      MultihashAlgorithmFactory[A]
  ): Option[Multihash] = digestValidated(bytes, algorithm).toOption
  def digest[A](bytes: Array[Byte], algorithm: A)(using
      MultihashAlgorithmFactory[A]
  ): Multihash =
    digestValidated(
      bytes,
      algorithm
    ).fold(error => throw ValidationError[Multihash](error), identity)

  extension (mh: Multihash)
    def =~(other: Multihash): Boolean = BigInt(mh).equals(BigInt(other))
    def !~(other: Multihash): Boolean = !(mh =~ other)

    def toBytes: Array[Byte] = mh

    def code: VarInt = VarInt.sequence(mh, 1).head.head
    def algorithm: MultihashAlgorithm = MultihashAlgorithm.getByCode(code).toOption.get

    def size: Int = VarInt.sequence(mh, 2).head.last.decode.toInt
    def digest: Array[Byte] = VarInt.sequence(mh, 2).last

    def toHumanReadable: String =
      VarInt.sequence(mh, 2) match
        case (Array(mhc, mhs), mhd) =>
          Vector(
            MultihashAlgorithm.getByCode(mhc).map(_.label).toOption.get,
            mhs.decode * 8,
            new String(Base16.encode(mhd))
          ).mkString("-")
        case (ba, via) =>
          throw ValidationError[Multihash](s"Unreachable case, bytes: ${ba}, varints: ${via}")

extension (sc: StringContext)
  def mh(args: Any*): Multihash = Multihash(sc.s(args*))
