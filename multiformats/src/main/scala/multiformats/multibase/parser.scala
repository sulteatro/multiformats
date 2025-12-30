package multiformats.multibase

import milletre.constructor.*
import milletre.utils.*
import multiencoder.encoder.*
import multiencoder.encoding.Encoding
import multiformats.varint.VarInt

// format: off
// generateMultibase: begin //
val multibaseReserved: Vector[Int] = Vector(0x0000, 0x002F, 0x0031, 0x0051)

enum MultibaseStatus:
  case `final`, draft, experimental

enum MultibaseAlgorithm(val unicode: Int, val description: Option[String], val status: MultibaseStatus):
  case base2             extends MultibaseAlgorithm(0x0030, Some("Binary (01010101)"), MultibaseStatus.experimental)
  case base8             extends MultibaseAlgorithm(0x0037, Some("Octal"), MultibaseStatus.draft)
  case base10            extends MultibaseAlgorithm(0x0039, Some("Decimal"), MultibaseStatus.draft)
  case base16            extends MultibaseAlgorithm(0x0066, Some("Hexadecimal (lowercase)"), MultibaseStatus.`final`)
  case base16upper       extends MultibaseAlgorithm(0x0046, Some("Hexadecimal (uppercase)"), MultibaseStatus.`final`)
  case base32hex         extends MultibaseAlgorithm(0x0076, Some("RFC4648 case-insensitive - no padding - highest char"), MultibaseStatus.experimental)
  case base32hexupper    extends MultibaseAlgorithm(0x0056, Some("RFC4648 case-insensitive - no padding - highest char"), MultibaseStatus.experimental)
  case base32hexpad      extends MultibaseAlgorithm(0x0074, Some("RFC4648 case-insensitive - with padding"), MultibaseStatus.experimental)
  case base32hexpadupper extends MultibaseAlgorithm(0x0054, Some("RFC4648 case-insensitive - with padding"), MultibaseStatus.experimental)
  case base32            extends MultibaseAlgorithm(0x0062, Some("RFC4648 case-insensitive - no padding"), MultibaseStatus.`final`)
  case base32upper       extends MultibaseAlgorithm(0x0042, Some("RFC4648 case-insensitive - no padding"), MultibaseStatus.`final`)
  case base32pad         extends MultibaseAlgorithm(0x0063, Some("RFC4648 case-insensitive - with padding"), MultibaseStatus.draft)
  case base32padupper    extends MultibaseAlgorithm(0x0043, Some("RFC4648 case-insensitive - with padding"), MultibaseStatus.draft)
  case base32z           extends MultibaseAlgorithm(0x0068, Some("z-base-32 (used by Tahoe-LAFS)"), MultibaseStatus.draft)
  case base36            extends MultibaseAlgorithm(0x006b, Some("Base36 [0-9a-z] case-insensitive - no padding"), MultibaseStatus.draft)
  case base36upper       extends MultibaseAlgorithm(0x004b, Some("Base36 [0-9a-z] case-insensitive - no padding"), MultibaseStatus.draft)
  case base45            extends MultibaseAlgorithm(0x0052, Some("Base45 RFC9285"), MultibaseStatus.draft)
  case base58btc         extends MultibaseAlgorithm(0x007a, Some("Base58 Bitcoin"), MultibaseStatus.`final`)
  case base58flickr      extends MultibaseAlgorithm(0x005a, Some("Base58 Flicker"), MultibaseStatus.experimental)
  case base64            extends MultibaseAlgorithm(0x006d, Some("RFC4648 no padding"), MultibaseStatus.`final`)
  case base64pad         extends MultibaseAlgorithm(0x004d, Some("RFC4648 with padding - MIME encoding"), MultibaseStatus.experimental)
  case base64url         extends MultibaseAlgorithm(0x0075, Some("RFC4648 no padding"), MultibaseStatus.`final`)
  case base64urlpad      extends MultibaseAlgorithm(0x0055, Some("RFC4648 with padding"), MultibaseStatus.`final`)
  case proquint          extends MultibaseAlgorithm(0x0070, Some("Proquint (https://arxiv.org/html/0901.4016)"), MultibaseStatus.experimental)
  case base256emoji      extends MultibaseAlgorithm(0x1F680, Some("base256 with custom alphabet using variable-sized-codepoints"), MultibaseStatus.experimental)

// generateMultibase: end //
// format: on

  def character: String = Character.toChars(this.unicode).mkString("")

  def encoder: Either[String, Encoding[?]] =
    this match
      case MultibaseAlgorithm.base2             => Right(Base2)
      case MultibaseAlgorithm.base8             => Right(Base8)
      case MultibaseAlgorithm.base10            => Right(Base10)
      case MultibaseAlgorithm.base16            => Right(Base16)
      case MultibaseAlgorithm.base16upper       => Right(Base16Upper)
      case MultibaseAlgorithm.base32hex         => Right(Base32Hex)
      case MultibaseAlgorithm.base32hexupper    => Right(Base32HexUpper)
      case MultibaseAlgorithm.base32hexpad      => Right(Base32HexPad)
      case MultibaseAlgorithm.base32hexpadupper => Right(Base32HexPadUpper)
      case MultibaseAlgorithm.base32            => Right(Base32)
      case MultibaseAlgorithm.base32upper       => Right(Base32Upper)
      case MultibaseAlgorithm.base32pad         => Right(Base32Pad)
      case MultibaseAlgorithm.base32padupper    => Right(Base32PadUpper)
      case MultibaseAlgorithm.base32z           => Right(Base32z)
      case MultibaseAlgorithm.base36            => Right(Base36)
      case MultibaseAlgorithm.base36upper       => Right(Base36Upper)
      case MultibaseAlgorithm.base58btc         => Right(Base58BTC)
      case MultibaseAlgorithm.base58flickr      => Right(Base58Flickr)
      case MultibaseAlgorithm.base64            => Right(Base64)
      case MultibaseAlgorithm.base64pad         => Right(Base64Pad)
      case MultibaseAlgorithm.base64url         => Right(Base64Url)
      case MultibaseAlgorithm.base64urlpad      => Right(Base64UrlPad)
      case code => Left(s"Multibase encoding not implemented: '$code'")

  def validate(bytes: Array[Byte]): Either[String, Array[Byte]] = encoder.flatMap(_.validate(bytes))
  def encode(bytes: Array[Byte]): Either[String, String] = encoder.map(_.encode(bytes))

object MultibaseAlgorithm:
  private lazy val nameToAlgorithm = MultibaseAlgorithm.values.map(a => a.toString -> a).toMap

  private lazy val charToAlgorithm = MultibaseAlgorithm.values.map(a => a.character -> a).toMap

  def getByName(name: String): Either[String, MultibaseAlgorithm] =
    nameToAlgorithm.get(name.toLowerCase).toRight(s"Unsupported multibase codec name: '$name'")

  def getByChar(char: String): Either[String, MultibaseAlgorithm] =
    charToAlgorithm.get(char).toRight(s"Unsupported multibase prefix character: '$char'")

  def getByCode(code: VarInt): Either[String, MultibaseAlgorithm] =
    val char: String = BigInt(code.decode).toByteArray.toUtf8
    charToAlgorithm.get(char).toRight(s"Unsupported multibase prefix code: '${code.toHex}' ($char)")

//
// Typeclass to get MultibaseAlgorithm values by their properties
//
private[multiformats] trait MultibaseAlgorithmFactory[C]
    extends EitherConversion[C, MultibaseAlgorithm]
private[multiformats] object MultibaseAlgorithmFactory:
  given MultibaseAlgorithmFactory[MultibaseAlgorithm] = v => Right(v)
  given MultibaseAlgorithmFactory[VarInt] = v => MultibaseAlgorithm.getByCode(v)
  given MultibaseAlgorithmFactory[String] =
    v =>
      if v.length.equals(1)
      then MultibaseAlgorithm.getByChar(v)
      else MultibaseAlgorithm.getByName(v)

//
// Input validators: MultibaseIngest
//
trait MultibaseIngest[V] extends EitherConversion[V, String]

object MultibaseIngest:

  // check that the tail string is a valid instance of the
  // multibase algorithm indicated by the head character
  given MultibaseIngest[String] =
    str =>
      MultibaseAlgorithm.getByChar(str.take(1))
        .flatMap(_.validate(str.drop(1).getBytes))
        .map(_ => str)

  // check that the provided bytes are a valid encoded
  // representation of the multibase algorithm provided
  given [A] => (af: MultibaseAlgorithmFactory[A]) => MultibaseIngest[(Array[Byte], A)] =
    (bytes, algorithm) => af(algorithm).flatMap(a => a.validate(bytes).map(a.character + _.toUtf8))

//
// Public object interface: Multibase
//
opaque type Multibase = String

object Multibase extends MultiConstructor[String, Multibase, MultibaseIngest]:

  //
  // Constructors that encode the provided bytes with the provided algorithm
  //
  def encodeValidated[C](value: Array[Byte], code: C)(using
      maf: MultibaseAlgorithmFactory[C]
  ): Either[String, Multibase] =
    maf(code).flatMap(algo => algo.encode(value).map(algo.character + _))
  def encodeIfValid[C](value: Array[Byte], code: C)(using
      c: MultibaseAlgorithmFactory[C]
  ): Option[Multibase] = encodeValidated(value, code).toOption
  def encode[C](value: Array[Byte], code: C)(using c: MultibaseAlgorithmFactory[C]): Multibase =
    encodeValidated(value, code).fold(error => throw ValidationError[Multibase](error), identity)

  extension (mb: Multibase)
    def =~(other: Multibase): Boolean = mb.equals(other)
    def !~(other: Multibase): Boolean = !(mb =~ other)

    def toBytes: Array[Byte] = mb.getBytes

    def prefix: String = mb.take(1)
    def data: String = mb.drop(1)
    def encoding: MultibaseAlgorithm = MultibaseAlgorithm.getByChar(prefix).toOption.get
    def encoder: Encoding[?] = encoding.encoder.toOption.get

    def decode: Array[Byte] = encoder.decode(data)

extension (sc: StringContext)
  def mb(args: Any*): Multibase = Multibase(sc.s(args*))
