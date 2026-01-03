package multiformats.varint

import milletre.constructor.ClearConstructor
import milletre.constructor.EitherConversion
import milletre.constructor.ValidationError

//
// Implementation details:
// * extractFromBytes attempts to extract a varint from a byte array starting at `start`
// * encodeFromLong attempts to encode an integer as a varint (all valid varints fit in a Long)
//
private val b0: Byte = 0.toByte

val MaxLengthInBytes: Int = 9
val BitEntropy: Int = 7

private def bigIntToByteArray(bi: BigInt): Array[Byte] =
  val barr = bi.toByteArray
  if barr.length > 1 && barr(0).equals(b0) then barr.drop(1) else barr

private def byteArrayAsString(bytes: Array[Byte], start: Int): String =
  bytes.drop(start).map(_.toString).mkString("Array(", ", ", ")")

private def extractFromBytes(value: Array[Byte], start: Int): Either[String, (VarInt, Int)] =
  Some(value.indexWhere(b => (b & 0x80).equals(0), start))
    .filter(_ > -1)
    .toRight(s"Invalid unsigned varint: '${byteArrayAsString(value, start)}'")
    .map(_ + 1)
    .filterOrElse(
      _ - start <= MaxLengthInBytes,
      s"Unsigned varint from ${byteArrayAsString(value, start)} exceeds ${MaxLengthInBytes} bytes"
    )
    .map(stop => (BigInt(1, value.slice(start, stop)), stop))

private def encodeFromLong(source: Long): Either[String, VarInt] =
  Right(source)
    .filterOrElse(_ >= 0L, "Negative integers cannot be encoded as unsigned varints")
    .map { uintSource =>
      Option.unless(uintSource.equals(0L)) {
        Iterator.iterate((uintSource, b0, 0)) {
          case (source, byte, _) =>
            (source, source >> BitEntropy) match
              case (0, 0) => (0, byte, 2)
              case (s, 0) => (0, (s & 0x7f).toByte, 1)
              case (s, n) => (n, (s | 0x80).toByte, 0)
        }.takeWhile(_._3 < 2).toArray.map(_.tail)
      }.getOrElse(Array((b0, 1)))
        .take(MaxLengthInBytes)
        .unzip
    }
    .filterOrElse(
      _._2.last.equals(1),
      s"Unsigned varint from '${source}' exceeds ${MaxLengthInBytes} bytes"
    )
    .map { case (barr, _) => BigInt(1, barr) }

type VarIntElements = (prefix: Array[Byte], result: Array[VarInt], suffix: Array[Byte])

private def extractSequence(
    source: Array[Byte],
    countOpt: Option[Int],
    start: Int
): Either[String, VarIntElements] =
  val byteSize: Int = source.size
  val (varints, stop) =
    Iterator.iterate((VarInt(0), start, 0)) {
      case (vi, index, nvarints) =>
        extractFromBytes(source, index).toOption.getOrElse((vi, byteSize + 1)) :* nvarints + 1
    }.drop(1).takeWhile {
      case (_, index, nvarints) =>
        !countOpt.exists(nvarints > _) && index < byteSize + 1
    }.map(_.init).toArray.unzip

  countOpt
    .filterNot(_ == varints.size)
    .fold(Right((source.take(start), varints, source.drop(stop.lastOption.getOrElse(0))))) {
      count =>
        Left(s"Cannot extract $count VarInt${if count > 1 then "s" else ""} starting at $start")
    }

trait VarIntIngest[V] extends EitherConversion[V, VarInt]
object VarIntIngest:
  given VarIntIngest[VarInt] = Right(_)

  given fromBytes: VarIntIngest[Array[Byte]] = extractFromBytes(_, 0).map(_._1)
  given fromBigInt: VarIntIngest[BigInt] = v => fromBytes(bigIntToByteArray(v))
  given VarIntIngest[Long] = v => fromBigInt(BigInt(v))
  given VarIntIngest[Int] = v => fromBigInt(BigInt(v))

trait VarIntEncode[V] extends EitherConversion[V, VarInt]
object VarIntEncode:
  given fromLong: VarIntEncode[Long] = encodeFromLong(_)
  given fromBigInt: VarIntEncode[BigInt] = v => fromLong(v.toLong)
  given VarIntEncode[Int] = v => fromLong(v.toLong)
  given VarIntEncode[Array[Byte]] = v => fromBigInt(BigInt(1, v))

trait VarIntSequence[V] extends EitherConversion[V, VarIntElements]
object VarIntSequence:
  given extract: VarIntSequence[(Array[Byte], Option[Int], Int)] = extractSequence.tupled(_)
  given VarIntSequence[(Array[Byte], Int)] = (s, c) => extract(s, Some(c), 0)
  given VarIntSequence[Array[Byte]] = extract(_, None, 0)

//
// Public object interface: VarInt
//
opaque type VarInt = BigInt

object VarInt extends ClearConstructor[VarInt, VarIntIngest]:

  //
  // Encoding constructors attempt to convert the value provided to a varint
  //
  def encodeValidated[T](value: T)(using encoder: VarIntEncode[T]): Either[String, VarInt] =
    encoder(value)
  def encodeIfValid[T](value: T)(using VarIntEncode[T]): Option[VarInt] =
    encodeValidated(value).toOption
  def encode[T](value: T)(using VarIntEncode[T]): VarInt =
    encodeValidated(value).fold(error => throw ValidationError[VarInt](error), identity)

  // Sequence constructors extract a sequence of varints from a byte array
  def sequenceValidated[T](value: T)(using seq: VarIntSequence[T]): Either[String, VarIntElements] =
    seq(value)
  def sequenceIfValid[T](value: T)(using VarIntSequence[T]): Option[VarIntElements] =
    sequenceValidated(value).toOption
  def sequence[T](value: T)(using VarIntSequence[T]): VarIntElements =
    sequenceValidated(value).fold(error => throw ValidationError[VarInt](error), identity)

  def fromValidated(source: Array[Byte], at: Int = 0): Either[String, VarInt] =
    summon[VarIntSequence[(Array[Byte], Int)]](source, at + 1).map(_.result.last)
  def fromIfValid[T](source: Array[Byte], start: Int = 0): Option[VarInt] =
    fromValidated(source, start).toOption
  def from[T](source: Array[Byte], start: Int = 0): VarInt =
    fromValidated(source, start).fold(error => throw ValidationError[VarInt](error), identity)

  extension (vi: VarInt)
    def =~(other: VarInt): Boolean = vi.equals(other)
    def !~(other: VarInt): Boolean = !(vi =~ other)

    def toBytes: Array[Byte] = bigIntToByteArray(vi)
    def toBigInt: BigInt = vi
    def toLong: Long = vi.toLong
    def toInt: Int = vi.toInt

    def toHex: String =
      val hs = toLong.toHexString
      "0x" + ("0" * (hs.length % 2)) + hs

    def toBinary: String =
      val bs = toLong.toBinaryString
      ("0" * ((8 - bs.length % 8) % 8) + bs).grouped(8).mkString(" ")

    def length: Int = toBytes.length

    def decode: Long =
      toBytes.reverse.foldLeft(0L) {
        case (result, b) => (result << BitEntropy) | (b & 0x7f)
      }
