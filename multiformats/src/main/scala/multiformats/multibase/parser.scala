package multiformats.multibase

import multiencoder.encoder.*
import multiencoder.encoding.Encoding
import multiformats.varint.VarInt

import java.nio.charset.StandardCharsets

final case class MultibaseValidationError(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

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

object MultibaseAlgorithm:
  private lazy val nameToAlgorithm = MultibaseAlgorithm.values.map(a => a.toString -> a).toMap

  def byName(name: String): Either[String, MultibaseAlgorithm] =
    nameToAlgorithm.get(name.toLowerCase).toRight(s"Unsupported multibase codec name: '$name'")

  private lazy val charToAlgorithm = MultibaseAlgorithm.values.map(a => a.character -> a).toMap

  def byChar(char: String): Either[String, MultibaseAlgorithm] =
    charToAlgorithm.get(char).toRight(s"Unsupported multibase prefix character: '$char'")

  def byCode(code: VarInt): Either[String, MultibaseAlgorithm] =
    val char: String = new String(BigInt(code.decode).toByteArray, StandardCharsets.UTF_8)
    charToAlgorithm.get(
      char
    ).toRight(s"Unsupported multibase prefix code: '${code.toHex}' ($char)")

//
// Implementation details:
// * extractFromBytes attempts to extract a varint from a byte array starting at `start`
// * encodeFromLong attempts to encode an integer as a varint (all valid varints fit in a Long)
//
private def validateMultibase(str: String): Either[String, Multibase] =
  MultibaseAlgorithm.byChar(str.take(1))
    .map { code =>
      code.encoder.map { encoder =>
        encoder
          .validate(str.drop(1))
          .map(_ => str)
      }.joinRight
    }.joinRight

private def validateWithBase(
    bytes: Array[Byte],
    code: MultibaseAlgorithm
): Either[String, Multibase] =
  code.encoder.map(_.validate(bytes)).joinRight.map(b =>
    code.character + new String(b, StandardCharsets.UTF_8)
  )

private def encodeWithBase(
    bytes: Array[Byte],
    code: MultibaseAlgorithm
): Either[String, Multibase] = code.encoder.map(enc => code.character + enc.encode(bytes))

//
// Type-variadic construction via typeclass
//
sealed trait MultibaseFactory[V, C]:
  def toString(Value: V): String
  def toBytes(Value: V): Array[Byte]
  def toMultibaseAlgorithm(Value: C): Either[String, MultibaseAlgorithm]

  def validate(value: V): Either[String, Multibase] = validateMultibase(toString(value))

  def checkCompat(value: V, code: C): Either[String, Multibase] =
    toMultibaseAlgorithm(code).map(mc => validateWithBase(toBytes(value), mc)).joinRight

  def encode(value: V, code: C): Either[String, Multibase] =
    toMultibaseAlgorithm(code).map(mc => encodeWithBase(toBytes(value), mc)).joinRight

object MultibaseFactory:
  given MultibaseFactory[String, MultibaseAlgorithm] =
    new MultibaseFactory[String, MultibaseAlgorithm]:
      def toString(value: String): String = value
      def toBytes(value: String): Array[Byte] = value.getBytes
      def toMultibaseAlgorithm(value: MultibaseAlgorithm): Either[String, MultibaseAlgorithm] =
        Right(value)

  given MultibaseFactory[Array[Byte], MultibaseAlgorithm] =
    new MultibaseFactory[Array[Byte], MultibaseAlgorithm]:
      def toString(value: Array[Byte]): String = new String(value, StandardCharsets.UTF_8)
      def toBytes(value: Array[Byte]): Array[Byte] = value
      def toMultibaseAlgorithm(value: MultibaseAlgorithm): Either[String, MultibaseAlgorithm] =
        Right(value)

  given MultibaseFactory[String, String] =
    new MultibaseFactory[String, String]:
      def toString(value: String): String = value
      def toBytes(value: String): Array[Byte] = value.getBytes
      def toMultibaseAlgorithm(value: String): Either[String, MultibaseAlgorithm] =
        if value.length.equals(1)
        then MultibaseAlgorithm.byChar(value)
        else MultibaseAlgorithm.byName(value)

  given MultibaseFactory[Array[Byte], String] =
    new MultibaseFactory[Array[Byte], String]:
      def toString(value: Array[Byte]): String = new String(value, StandardCharsets.UTF_8)
      def toBytes(value: Array[Byte]): Array[Byte] = value
      def toMultibaseAlgorithm(value: String): Either[String, MultibaseAlgorithm] =
        if value.length.equals(1)
        then MultibaseAlgorithm.byChar(value)
        else MultibaseAlgorithm.byName(value)

opaque type Multibase = String

object Multibase:

  //
  // Constructors that validate and type an existing multibase string
  //
  def validated[V](value: V)(using
      c: MultibaseFactory[V, MultibaseAlgorithm]
  ): Either[String, Multibase] = c.validate(value)
  def ifValid[V](value: V)(using c: MultibaseFactory[V, MultibaseAlgorithm]): Option[Multibase] =
    c.validate(value).toOption
  def apply[V](value: V)(using c: MultibaseFactory[V, MultibaseAlgorithm]): Multibase =
    c.validate(value).fold(error => throw MultibaseValidationError(error), identity)

  //
  // Constructors that build a multibase string from an encoded string and a MultibaseAlgorithm
  //
  def validated[V, C](value: V, code: C)(using
      c: MultibaseFactory[V, C]
  ): Either[String, Multibase] = c.checkCompat(value, code)
  def ifValid[V, C](value: V, code: C)(using c: MultibaseFactory[V, C]): Option[Multibase] =
    c.checkCompat(value, code).toOption
  def apply[V, C](value: V, code: C)(using c: MultibaseFactory[V, C]): Multibase =
    c.checkCompat(value, code).fold(error => throw MultibaseValidationError(error), identity)

  //
  // Constructors that encode the provided bytes with the provided algorithm
  //
  def encodeValidated[V, C](value: V, code: C)(using
      c: MultibaseFactory[V, C]
  ): Either[String, Multibase] = c.encode(value, code)
  def encodeIfValid[V, C](value: V, code: C)(using c: MultibaseFactory[V, C]): Option[Multibase] =
    c.encode(value, code).toOption
  def encode[V, C](value: V, code: C)(using c: MultibaseFactory[V, C]): Multibase =
    c.encode(value, code).fold(error => throw MultibaseValidationError(error), identity)

  extension (mb: Multibase)
    def =~(other: Multibase): Boolean = mb.equals(other)
    def !~(other: Multibase): Boolean = !(mb =~ other)

    def toBytes: Array[Byte] = mb.getBytes

    def prefix: String = mb.take(1)
    def data: String = mb.drop(1)
    def encoding: MultibaseAlgorithm = MultibaseAlgorithm.byChar(prefix).toOption.get
    def encoder: Encoding[?] = encoding.encoder.toOption.get

    def decode: Array[Byte] = encoder.decode(data)

extension (sc: StringContext)
  def mb(args: Any*): Multibase = Multibase(sc.s(args*))
