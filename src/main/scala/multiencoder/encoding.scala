package multiencoder

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

import array.SizedArray
import Math.log10

object encoding:

  // Compute the greatest common denominator between two integers
  @tailrec
  private def gcd(a: Int, b: Int): Int = if b == 0 then a else gcd(b, a % b)

  // Given a value `n`, return the amount to add to it to make it divisible by `packSize`
  private inline def padSize(n: Int, packSize: Int): Int = (packSize - (n % packSize)) % packSize

  private inline def bitEntropy(base: Int): Double = log10(base) / log10(2)

  private inline def bytesToString(bytes: Array[Byte]): String =
    new String(bytes, StandardCharsets.UTF_8)

  enum PackingOrientation:
    case LeftOriented, RightOriented

  // Base encoding interface, defining core components needed by every implementation
  sealed trait Encoding[Base <: Int](using Base: ValueOf[Base]):

    // Encoding alphabet and derived lookup structures
    val alphabet: SizedArray[Base, Char]

    // Get and set the padding orientation of the encoding algorithm
    val orientation: PackingOrientation
    def asLeftOriented: Encoding[Base]
    def asRightOriented: Encoding[Base]

    // Get and set the pad character of the encoding algorithm
    val padChar: Option[Char]
    def withPadChar(p: Char): Encoding[Base]
    def withNoPadChar: Encoding[Base]

    protected lazy val alphEncode: Map[Byte, Byte] =
      alphabet.toArray.zipWithIndex.map { case (c, i) => i.toByte -> c.toByte }.toMap
    protected lazy val alphDecode: Map[Byte, Byte] =
      alphabet.toArray.zipWithIndex.map { case (c, i) => c.toByte -> i.toByte }.toMap

    // Representations of this encoding's bit entropy and base (2^entropy)
    val base: Base = Base.value

    def encode(barr: Array[Byte]): String
    def decode(barr: String): Array[Byte]
    def validate(barr: Array[Byte]): Either[String, Array[Byte]]

    def validate(str: String): Either[String, String] = validate(str.getBytes).map(bytesToString)

  /** Notes on this algorithm:
    *
    * The `entropy` of an encoding refers to the number of bits represented by the provided
    * alphabet. The number of integers representable by those bits is the `base`, the value which
    * defines the encoding (as 2 ^ entropy; e.g. base 64 is 2 ^ 6, so 6 bits define the characters
    * available).
    *
    * As each byte is 8 bits, the number of bytes processed per group must result in a number of
    * bits divisible by the entropy; thus, we determine the GCD of 8 and the entropy and divide 8 by
    * this value to get the minimum number of `packBytes` for which the number of entropy-bit
    * `packChars` is an integer (e.g. base 64's 6 bits and 8 share a GCD of 2, so 6 / 2 = 3 groups
    * of bytes makes 8 / 2 = 4 encoding characters).
    *
    * If a padding character is defined for an Encoding instance, that character is appended to any
    * encoded output to ensure the resulting length is a multiple of the number of encoding
    * characters in a groups. For instance, in base 64, a 10-character encoding will have two
    * padding characters appended to create a 12-character output.
    */
  case class BitEncoding[Base <: Int](
      alphabet: SizedArray[Base, Char],
      orientation: PackingOrientation,
      padChar: Option[Char]
  )(using Base: ValueOf[Base]) extends Encoding[Base]:
    padChar.filter(alphabet.contains).foreach(p =>
      throw new IllegalArgumentException(
        s"Pad character '$p' cannot be a member of the encoding alphabet"
      )
    )

    import PackingOrientation.*

    // This encoding can only be used if the base is a power of 2
    // Here, we check this while computing its bit entropy
    val entropy: Int =
      val result: Double = bitEntropy(base)
      require(result == result.toInt, s"Encoding base '$base' is not a power of 2")
      result.toInt

    // Padding character:
    // * An optional padding character can be set on this instance. If it is present, encoding a
    //   byte array will either prepend or append as many copies of this character to the resulting
    //   encoded array as are needed to make it fill the internal integer-packed representation.
    private lazy val padByte: Option[Byte] = padChar.map(_.toByte)
    def withPadChar(p: Char): BitEncoding[Base] = this.copy(padChar = Some(p))
    def withNoPadChar: BitEncoding[Base] = this.copy(padChar = None)

    // Packing orientation:
    // * The packing orientation (left or right) determines how bits are packed into integers during
    //   encoding, and which end of an encoded byte array will be padded. This also applies to the
    //   internal representation; e.g. for LeftOriented, padding characters will be prepended, and
    //   the decoder will treat 0-bits on the left end of the endcoded array as padding.
    def asLeftOriented: Encoding[Base] = this.copy(orientation = LeftOriented)
    def asRightOriented: Encoding[Base] = this.copy(orientation = RightOriented)

    private val (packBytes, packChars) =
      Some(gcd(8, entropy)).map(gcd => (entropy / gcd, 8 / gcd)).get

    // Extract encoded bytes using fast bitwise operations; only valid if the base is a power of 2
    private def encodeBytes(packedBarr: Array[Byte]): Array[Byte] =
      packedBarr
        .grouped(packBytes)
        .map(_.foldLeft(0L) { case (pack, b) => (pack << 8) | (b & 0xff) })
        .flatMap(pack =>
          ((packChars - 1) * entropy to 0 by -entropy).map(s => (pack >>> s) & (base - 1))
        )
        .map(_.toByte)
        .toArray

    // Given a number of non-encoded bytes, return the minimum number of bytes in its encoding
    private inline def encodedSize(n: Int): Int = (packChars * n + packBytes - 1) / packBytes

    // Encode the provided byte array and return a byte array drawn from the encoding alphabet
    // using this Encoding instance's padding options.
    def encode(barr: Array[Byte]): String =
      val inputPad: Array[Byte] = Array.fill[Byte](padSize(barr.size, packBytes))(0)
      val outputSize: Int = encodedSize(barr.size)
      if orientation.equals(LeftOriented) then
        val (toPad, enc) = encodeBytes(inputPad ++ barr).splitAt(padSize(outputSize, packChars))
        bytesToString(toPad.flatMap(_ => padByte) ++ enc.map(alphEncode.apply))
      else
        val (enc, toPad) = encodeBytes(barr ++ inputPad).splitAt(outputSize)
        bytesToString(enc.map(alphEncode.apply) ++ toPad.flatMap(_ => padByte))

    // Implementation of `decode` for an encoded byte array padded to ensure byte packs of equal size
    private def decodePacks(packedBarr: Array[Byte]): Array[Byte] =
      packedBarr
        .grouped(packChars)
        .map(_.foldLeft(0L) { case (pack, b) => (pack << entropy) | (b & (base - 1)) })
        .flatMap(pack => ((packBytes - 1) * 8 to 0 by -8).map(s => (pack >>> s) & 0xff))
        .map(_.toByte)
        .toArray

    // Given a number of encoded bytes, return the minimum number of bytes it encodes
    private inline def decodedSize(n: Int): Int = (packBytes * n) / packChars

    // Decode an array of bytes that are members of this object's alphabet into the "original" byte
    // array in which the full 8 bits available in a byte are used.
    def decode(str: String): Array[Byte] =
      val barr = str.getBytes
      if barr.size == 0 then
        Array.empty[Byte]
      else if orientation.equals(LeftOriented) then
        val strippedBarr = padByte.fold(barr)(pb => barr.dropWhile(_ == pb)).map(alphDecode.apply)
        val (outputPadSize, outputSize) =
          Some(strippedBarr.size).map(es => (padSize(es, packChars), decodedSize(es))).get
        decodePacks(Array.fill[Byte](outputPadSize)(0) ++ strippedBarr).takeRight(outputSize)
      else
        val strippedBarr = padByte.fold(barr)(pb => barr.takeWhile(_ != pb)).map(alphDecode.apply)
        val (outputPadSize, outputSize) =
          Some(strippedBarr.size).map(es => (padSize(es, packChars), decodedSize(es))).get
        decodePacks(strippedBarr ++ Array.fill[Byte](outputPadSize)(0)).take(outputSize)

    // Validate that a provided byte array can be decoded by this instance
    def validate(barr: Array[Byte]): Either[String, Array[Byte]] =
      val nonPadCharsEither: Either[String, Array[Byte]] =
        padByte.fold[Either[String, Array[Byte]]](Right(barr)) { pb =>
          if orientation.equals(LeftOriented) then
            if pb == barr.last then
              Left(s"Found right padding with '${pb.toChar}' instead of left padding")
            else if padSize(barr.size, packChars) != 0 then
              Left(s"Missing expected left padding with '${pb.toChar}'")
            else
              Right(barr.dropWhile(_ == pb))
          else
            if pb == barr.head then
              Left(s"Found left padding with '${pb.toChar}' instead of right padding")
            else if padSize(barr.size, packChars) != 0 then
              Left(s"Missing expected right padding with '${pb.toChar}'")
            else
              Right(barr.takeWhile(_ != pb))
        }

      nonPadCharsEither.filterOrElse(
        _.toSet.subsetOf(alphEncode.values.toSet),
        "Invalid encoding characters"
      ).map(_ => barr)

  case class IntEncoding[Base <: Int](
      alphabet: SizedArray[Base, Char]
  )(using Base: ValueOf[Base]) extends Encoding[Base]:
    import PackingOrientation.*

    // This encoding approach does not depend on any property of the bit entropy
    // so we define it here as a double for convenience
    val entropy: Double = bitEntropy(base)

    // Padding is not applicable here; make these no-ops
    val padChar: Option[Char] = None
    def withPadChar(p: Char): Encoding[Base] = this
    def withNoPadChar: Encoding[Base] = this

    // Orientation is also not applicable here; make these no-ops
    val orientation: PackingOrientation = LeftOriented
    def asLeftOriented: IntEncoding[Base] = this
    def asRightOriented: IntEncoding[Base] = this

    // Extract encoded bytes by packing a BigInt and using repeated div & mod; used if the base is
    // not a power of 2.
    private def encodeInt(packedBarr: Array[Byte]): Array[Byte] =
      Iterator.iterate((Option.empty[Byte], BigInt(1, packedBarr), true)) {
        case (output, num, _) => (Some((num % base).toByte), num / base, num > 0)
      }.takeWhile(_._3).flatMap(_._1).toArray.reverse

    // Encode the provided byte array and return a byte array drawn from the encoding alphabet
    // Regardless of orientation, we have to manually preserve leading zeros because the internal
    // mechanism converts the byte array to an integer, thus ignoring them.
    def encode(barr: Array[Byte]): String =
      val (inputPad, packedBarr) = barr.splitAt(barr.indexWhere(_ != 0))
      val encodedBytes: Array[Byte] = inputPad ++ encodeInt(packedBarr)
      bytesToString(encodedBytes.map(alphEncode.apply))

    private def decodeInt(packedBarr: Array[Byte]): Array[Byte] =
      packedBarr.foldLeft(BigInt(0)) {
        case (output, num) => output * base + num.toInt
      }.toByteArray.dropWhile(_ == 0)

    // Decode the provided byte array using the encoding alphabet and return the original byte array
    // including any leading (left-oriented) or trailing (right-oriented) zeros
    def decode(str: String): Array[Byte] =
      val encodedBytes = str.getBytes.map(alphDecode.apply)
      val (inputPad, packedBarr) = encodedBytes.splitAt(encodedBytes.indexWhere(_ != 0))
      inputPad ++ decodeInt(packedBarr)

    // Validate that a provided byte array can be decoded by this instance
    def validate(barr: Array[Byte]): Either[String, Array[Byte]] =
      Right(barr).filterOrElse(
        _.toSet.subsetOf(alphEncode.values.toSet),
        "Invalid encoding characters"
      )

  object Encoding:
    import PackingOrientation.*

    def apply[Size <: Int](
        encodingAlphabet: SizedArray[Size, Char],
        padCharOpt: Option[Char] = None,
        leftOriented: Boolean = false
    )(using Size: ValueOf[Size]): Encoding[Size] =
      val entropy = bitEntropy(Size.value)
      if !entropy.toInt.toDouble.equals(entropy) then
        throw new IllegalArgumentException(
          f"Padding & orientation cannot be applied to encoding with alphabet size ${Size.value}"
        )

      val orientation = if leftOriented then LeftOriented else RightOriented
      BitEncoding[Size](encodingAlphabet, orientation, padCharOpt)

    def apply[Size <: Int](
        encodingAlphabet: SizedArray[Size, Char]
    )(using Size: ValueOf[Size]): Encoding[Size] =
      val entropy = bitEntropy(Size.value)
      if entropy.toInt.toDouble.equals(entropy) then
        BitEncoding[Size](encodingAlphabet, RightOriented, None)
      else
        IntEncoding[Size](encodingAlphabet)
