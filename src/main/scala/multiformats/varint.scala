package multiformats

final case class VarIntValidationError(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

object varint:

  //
  // Implementation details:
  // * extractFromBytes attempts to extract a varint from a byte array starting at `start`
  // * encodeFromLong attempts to encode an integer as a varint (all valid varints fit in a Long)
  //
  val b0: Byte = 0.toByte
  val MaxLengthInBytes: Int = 9
  val BitEntropy: Int = 7

  private def bigIntToByteArray(bi: BigInt): Array[Byte] =
    val barr = bi.toByteArray
    if barr.length > 1 && barr(0).equals(b0) then barr.drop(1) else barr

  private def byteArrayAsString(bytes: Array[Byte], start: Int): String =
    bytes.drop(start).map(_.toString).mkString("Array(", ", ", ")")

  def extractFromBytes(value: Array[Byte], start: Int): Either[String, (VarInt, Int)] =
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

  //
  // Type-variadic construction via typeclass
  //
  sealed trait VarIntFactory[V]:
    def toByteArray(value: V): Array[Byte]
    def toLong(value: V): Long

    def validate(value: V): Either[String, VarInt] =
      extractFromBytes(toByteArray(value), 0).map(_._1)
    def encode(value: V): Either[String, VarInt] = encodeFromLong(toLong(value))

  private object VarIntFactory:
    given VarIntFactory[Array[Byte]] =
      new VarIntFactory[Array[Byte]]:
        def toByteArray(value: Array[Byte]): Array[Byte] = value
        def toLong(value: Array[Byte]): Long = BigInt(1, value).toLong

    given VarIntFactory[BigInt] =
      new VarIntFactory[BigInt]:
        def toByteArray(value: BigInt): Array[Byte] = bigIntToByteArray(value)
        def toLong(value: BigInt): Long = value.toLong

    given VarIntFactory[Long] =
      new VarIntFactory[Long]:
        def toByteArray(value: Long): Array[Byte] = bigIntToByteArray(BigInt(value))
        def toLong(value: Long): Long = value

    given VarIntFactory[Int] =
      new VarIntFactory[Int]:
        def toByteArray(value: Int): Array[Byte] = bigIntToByteArray(BigInt(value))
        def toLong(value: Int): Long = value.toLong

  //
  // Public object interface: VarInt
  //
  opaque type VarInt = BigInt

  object VarInt:
    import VarIntFactory.given

    //
    // Basic constructors check that the value provided is a valid varint
    //
    def validated[T](value: T)(using c: VarIntFactory[T]): Either[String, VarInt] =
      c.validate(value)
    def ifValid[T](value: T)(using c: VarIntFactory[T]): Option[VarInt] = c.validate(value).toOption
    def apply[T](value: T)(using c: VarIntFactory[T]): VarInt =
      c.validate(value).fold(error => throw VarIntValidationError(error), identity)

    //
    // Encoding constructors attempt to convert the value provided to a varint
    //
    def encodeValidated[T](value: T)(using c: VarIntFactory[T]): Either[String, VarInt] =
      c.encode(value)
    def encodeIfValid[T](value: T)(using c: VarIntFactory[T]): Option[VarInt] =
      c.encode(value).toOption
    def encode[T](value: T)(using c: VarIntFactory[T]): VarInt =
      c.encode(value).fold(error => throw VarIntValidationError(error), identity)

    // Sequence constructors extract a sequence of varints from a byte array
    def sequence(
        source: Array[Byte],
        count: Option[Int],
        start: Int
    ): (Array[Byte], Array[VarInt], Array[Byte]) =
      val byteSize: Int = source.size
      val (varints, stop) =
        Iterator.iterate((VarInt(0), start, 0)) {
          case (vi, index, nvarints) =>
            extractFromBytes(source, index).toOption.getOrElse((vi, byteSize + 1)) :* nvarints + 1
        }.drop(1).takeWhile {
          case (_, index, nvarints) =>
            !count.exists(nvarints > _) && index < byteSize + 1
        }.map(_.init).toArray.unzip

      (source.take(start), varints, source.drop(stop.lastOption.getOrElse(0)))

    def sequence(source: Array[Byte], count: Int): (Array[VarInt], Array[Byte]) =
      sequence(source, Some(count), 0).tail

    def sequence(source: Array[Byte]): Array[VarInt] = sequence(source, None, 0)._2

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
