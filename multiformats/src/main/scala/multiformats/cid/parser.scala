package multiformats.cid

import multiformats.multibase.Multibase
import multiformats.multibase.MultibaseAlgorithm
import multiformats.multibase.MultibaseFactory
import multiformats.multicodec.Multicodec
import multiformats.multicodec.MulticodecFactory
import multiformats.multicodec.MulticodecTag
import multiformats.multihash.Multihash
import multiformats.multihash.MultihashAlgorithm
import multiformats.multihash.MultihashFactory
import multiformats.varint.VarInt

final case class CIDValidationError(
    private val message: String = "",
    private val cause: Throwable = None.orNull
) extends Exception(message, cause)

val version: Multicodec = Multicodec.cidv1

//
// Implementation details:
// * parseCID: attempt to interpret a byte array as a content-type multicodec and multihash
// * buildCID: construct a CIDv1 from a content type multicodec and content address bytes
// * digestCID: construct a CIDv1 as in buildCID, but first hashing content bytes into an address
// * translateCID: encode a human-readable CID into its standard binary representation
//

private def parseCID(cid: Array[Byte]): Either[String, (Multicodec, Multihash)] =
  VarInt.sequence(cid, 2) match
    case (Array(cidCode, contentCode), mhBytes) =>
      Multicodec.validated(cidCode).map { cidCodec =>
        Multicodec.validated(contentCode).map((cidCodec, _))
      }.joinRight
        .filterOrElse(
          _.head.equals(version),
          s"Invalid CID multicodec code: '${cidCode.toHex}'"
        ).filterOrElse(
          _.last.tag.equals(MulticodecTag.ipld),
          s"Invalid content-type multicodec code: '${contentCode.toHex}'"
        ).map { case (cidCodec, contentCodec) =>
          Multihash.validated(mhBytes).map((contentCodec, _))
        }.joinRight
    case _ =>
      Left("Invalid CID format: could not extract Multicodec code varints")

private def buildCID(
    contentType: Multicodec,
    contentAddress: Multihash
): Either[String, Array[Byte]] =
  if contentType.tag.equals(MulticodecTag.ipld) then
    Right(version.code.toBytes ++ contentType.code.toBytes ++ contentAddress.toBytes)
  else
    Left(s"Invalid content-type multicodec code: '${contentType.code.toHex}'")

private def digestCID(
    content: Array[Byte],
    contentType: Multicodec,
    hashAlgorithm: MultihashAlgorithm
): Either[String, Array[Byte]] =
  Multihash.digestValidated(content, hashAlgorithm).map(buildCID(contentType, _)).joinRight

private def translateCID(hr: String)(using
    MultibaseFactory[Array[Byte], MultibaseAlgorithm]
): Either[String, Multibase] =
  hr.split(" - ") match
    case Array(baseName, cidCodec, typeName, address) if cidCodec.equals(version.toString) =>
      MultibaseAlgorithm.byName(baseName).map { baseAlgorithm =>
        Multicodec.validated(typeName).map { contentType =>
          Multihash.validated(address).map { contentAddress =>
            buildCID(contentType, contentAddress).map(
              Multibase.encodeValidated(_, baseAlgorithm)
            ).joinRight
          }.joinRight
        }.joinRight
      }.joinRight
    case _ =>
      Left(s"Invalid human-readable CID format: '$hr'")

//
// The core CID definition is binary, but it has two String variatns: a multibase-encoded version
// and a human-readable string verison. Here, we define CID-specific types to indicate which
// definition is used internally - CID[Raw] is binary while CID[Encoded] is a Multibase object.
//
sealed trait CIDState
sealed trait Raw extends CIDState
sealed trait Encoded extends CIDState

private type CIDStateRepr[S] = S match
  case Raw     => Array[Byte]
  case Encoded => Multibase

//
// Type-variadic conversion from a single type, via typeclass
//
sealed trait CIDConverterFactory[V]:
  def convert(value: V): Either[String, Multibase]

object CIDConverterFactory:
  given CIDConverterFactory[Multibase] =
    new CIDConverterFactory[Multibase]:
      def convert(value: Multibase): Either[String, Multibase] =
        parseCID(value.decode).map(_ => value)

  given CIDConverterFactory[String] =
    new CIDConverterFactory[String]:
      def convert(value: String): Either[String, Multibase] =
        if value.contains(" - ") then
          translateCID(value)
        else
          Multibase.validated(value)
            .map(mb => parseCID(mb.decode).map(_ => mb))
            .joinRight

//
// Type-variadic construction from content type (T), content address (A),
// and optional base encoding (B) via typeclass
//
sealed trait CIDConstructorFactory[A]:
  def toMultihash(value: A): Either[String, Multihash]

  private def convertRawArguments[T](typeSource: T, addressSource: A)(using
      mcf: MulticodecFactory[T]
  ): Either[String, (Multicodec, Multihash)] =
    mcf.convert(typeSource).map { contentType =>
      toMultihash(addressSource).map((contentType, _))
    }.joinRight

  def constructRaw[T](typeSource: T, addressSource: A)(using
      MulticodecFactory[T]
  ): Either[String, Array[Byte]] =
    convertRawArguments(typeSource, addressSource).map { case (contentType, contentAddress) =>
      buildCID(contentType, contentAddress)
    }.joinRight

  def constructEncoded[T, B](typeSource: T, addressSource: A, base: B)(using
      MulticodecFactory[T],
      MultibaseFactory[Array[Byte], B]
  ): Either[String, Multibase] =
    convertRawArguments(typeSource, addressSource).map { case (contentType, contentAddress) =>
      buildCID(contentType, contentAddress).map(Multibase.encodeValidated(_, base)).joinRight
    }.joinRight

object CIDConstructorFactory:
  given CIDConstructorFactory[Multihash] =
    new CIDConstructorFactory[Multihash]:
      def toMultihash(value: Multihash): Either[String, Multihash] = Right(value)

  given CIDConstructorFactory[Array[Byte]] =
    new CIDConstructorFactory[Array[Byte]]:
      def toMultihash(value: Array[Byte]): Either[String, Multihash] = Multihash.validated(value)

//
// Type-variadic digestion of content into CID from content (C), hashing algorithm (H),
// optional content type (T), and optional base encoding (B) via typeclass
//
sealed trait CIDDigestorFactory[C]:
  def toByteArray(value: C): Array[Byte]

  private def convertRawArguments[T, H](typeSource: T, hashSource: H)(using
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H]
  ): Either[String, (Multicodec, MultihashAlgorithm)] =
    mcf.convert(typeSource).map { contentType =>
      mhf.toMultihashAlgorithm(hashSource).map((contentType, _))
    }.joinRight

  def digestRaw[T, H](contentSource: C, typeSource: T, hashSource: H)(using
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H]
  ): Either[String, Array[Byte]] =
    convertRawArguments(typeSource, hashSource).map { case (contentType, hashAlgorithm) =>
      digestCID(toByteArray(contentSource), contentType, hashAlgorithm)
    }.joinRight

  def digestEncoded[T, H, B](contentSource: C, typeSource: T, hashSource: H, base: B)(using
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H],
      mbf: MultibaseFactory[Array[Byte], B]
  ): Either[String, Multibase] =
    convertRawArguments(typeSource, hashSource).map { case (contentType, hashAlgorithm) =>
      digestCID(toByteArray(contentSource), contentType, hashAlgorithm).map(
        Multibase.encodeValidated(_, base)
      ).joinRight
    }.joinRight

object CIDDigestorFactory:
  given CIDDigestorFactory[Array[Byte]] =
    new CIDDigestorFactory[Array[Byte]]:
      def toByteArray(value: Array[Byte]): Array[Byte] = value

  given CIDDigestorFactory[String] =
    new CIDDigestorFactory[String]:
      def toByteArray(value: String): Array[Byte] = value.getBytes

  given CIDDigestorFactory[Multibase] =
    new CIDDigestorFactory[Multibase]:
      def toByteArray(value: Multibase): Array[Byte] = value.decode

//
// Public object interface: CID[Raw] and CID[Encoded]
//
opaque type CID[S] = CIDStateRepr[S]

object CID:

  //
  // Constructors that validate and type an existing object as a CID, choosing Raw or Encoded
  // according to the input type
  //
  def validated(value: Array[Byte]): Either[String, CID[Raw]] = parseCID(value).map(_ => value)
  def ifValid(value: Array[Byte]): Option[CID[Raw]] = validated(value).toOption
  def apply(value: Array[Byte]): CID[Raw] =
    validated(value).fold(error => throw CIDValidationError(error), identity)

  def validated[V](value: V)(using c: CIDConverterFactory[V]): Either[String, CID[Encoded]] =
    c.convert(value)
  def ifValid[V](value: V)(using c: CIDConverterFactory[V]): Option[CID[Encoded]] =
    c.convert(value).toOption
  def apply[V](value: V)(using c: CIDConverterFactory[V]): CID[Encoded] =
    c.convert(value).fold(error => throw CIDValidationError(error), identity)

  //
  // Constructors that build a CID[Raw] if content type and address are provided,
  // or CID[Encoded] if a base algorithm is also provided
  //
  def validated[T, A](contentType: T, address: A)(using
      c: CIDConstructorFactory[A],
      mcf: MulticodecFactory[T]
  ): Either[String, CID[Raw]] = c.constructRaw(contentType, address)

  def ifValid[T, A](contentType: T, address: A)(using
      c: CIDConstructorFactory[A],
      mcf: MulticodecFactory[T]
  ): Option[CID[Raw]] = c.constructRaw(contentType, address).toOption

  def apply[T, A](contentType: T, address: A)(using
      c: CIDConstructorFactory[A],
      mcf: MulticodecFactory[T]
  ): CID[Raw] =
    c.constructRaw(contentType, address).fold(error => throw CIDValidationError(error), identity)

  def validated[T, A, B](contentType: T, address: A, base: B)(using
      c: CIDConstructorFactory[A],
      mcf: MulticodecFactory[T],
      mbf: MultibaseFactory[Array[Byte], B]
  ): Either[String, CID[Encoded]] = c.constructEncoded(contentType, address, base)

  def ifValid[T, A, B](contentType: T, address: A, base: B)(using
      c: CIDConstructorFactory[A],
      mcf: MulticodecFactory[T],
      mbf: MultibaseFactory[Array[Byte], B]
  ): Option[CID[Encoded]] = c.constructEncoded(contentType, address, base).toOption

  def apply[T, A, B](contentType: T, address: A, base: B)(using
      c: CIDConstructorFactory[A],
      mcf: MulticodecFactory[T],
      mbf: MultibaseFactory[Array[Byte], B]
  ): CID[Encoded] =
    c.constructEncoded(contentType, address, base)
      .fold(error => throw CIDValidationError(error), identity)

  //
  // Constructors that digest some provided content into an address with the given hash algorithm
  // then build a CID[Raw] if just content type is provided, or CID[Encoded] if a base algorithm
  // is also provided
  //
  def digestValidated[C, T, H](content: C, contentType: T, hash: H)(using
      c: CIDDigestorFactory[C],
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H]
  ): Either[String, CID[Raw]] = c.digestRaw(content, contentType, hash)

  def digestIfValid[C, T, H](content: C, contentType: T, hash: H)(using
      c: CIDDigestorFactory[C],
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H]
  ): Option[CID[Raw]] = c.digestRaw(content, contentType, hash).toOption

  def digest[C, T, H](content: C, contentType: T, hash: H)(using
      c: CIDDigestorFactory[C],
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H]
  ): CID[Raw] =
    c.digestRaw(content, contentType, hash).fold(
      error => throw CIDValidationError(error),
      identity
    )

  def digestValidated[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
      c: CIDDigestorFactory[C],
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H],
      mbf: MultibaseFactory[Array[Byte], B]
  ): Either[String, CID[Encoded]] = c.digestEncoded(content, contentType, hash, base)

  def digestIfValid[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
      c: CIDDigestorFactory[C],
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H],
      mbf: MultibaseFactory[Array[Byte], B]
  ): Option[CID[Encoded]] = c.digestEncoded(content, contentType, hash, base).toOption

  def digest[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
      c: CIDDigestorFactory[C],
      mcf: MulticodecFactory[T],
      mhf: MultihashFactory[H],
      mbf: MultibaseFactory[Array[Byte], B]
  ): CID[Encoded] =
    c.digestEncoded(content, contentType, hash, base)
      .fold(error => throw CIDValidationError(error), identity)

  extension (cidRaw: CID[Raw])
    def toBytes: Array[Byte] = cidRaw

    def encode[B](base: B)(using MultibaseFactory[Array[Byte], B]): CID[Encoded] =
      Multibase.encode(toBytes, base)

    def =~(other: CID[Raw]): Boolean = toBytes.toSeq.equals(other.toBytes.toSeq)
    def =~(other: CID[Encoded]): Boolean = cidRaw =~ other.decode
    def !~(other: CID[Raw]): Boolean = !(cidRaw =~ other)
    def !~(other: CID[Encoded]): Boolean = !(cidRaw =~ other)

    def codec: Multicodec = version
    def contentType: Multicodec = parseCID(cidRaw).map(_.head).toOption.get
    def address: Multihash = parseCID(cidRaw).map(_.last).toOption.get

    def isAddressOf[C](content: C)(using
        CIDDigestorFactory[C],
        MulticodecFactory[Multicodec],
        MultihashFactory[MultihashAlgorithm]
    ): Boolean = cidRaw =~ CID.digest(content, contentType, address.algorithm)

  extension (cidEncoded: CID[Encoded])
    def toMultibase: Multibase = cidEncoded
    def toBytes: Array[Byte] = cidEncoded.toString.getBytes

    def decode: CID[Raw] = cidEncoded.encoder.decode(cidEncoded.data)

    def =~(other: CID[Encoded]): Boolean = cidEncoded.equals(other.toMultibase)
    def =~(other: CID[Raw]): Boolean = decode =~ other
    def !~(other: CID[Encoded]): Boolean = !(cidEncoded =~ other)
    def !~(other: CID[Raw]): Boolean = !(cidEncoded =~ other)

    def codec: Multicodec = version
    def contentType: Multicodec = parseCID(cidEncoded.decode).map(_.head).toOption.get
    def address: Multihash = parseCID(cidEncoded.decode).map(_.last).toOption.get
    def encoding: MultibaseAlgorithm = MultibaseAlgorithm.byChar(cidEncoded.prefix).toOption.get

    def toHumanReadable: String =
      Vector(
        encoding.toString,
        version.toString,
        contentType.toString,
        address.toHumanReadable
      ).mkString(" - ")

    def isAddressOf[C](content: C)(using
        CIDDigestorFactory[C],
        MulticodecFactory[Multicodec],
        MultihashFactory[MultihashAlgorithm]
    ): Boolean = cidEncoded =~ CID.digest(content, contentType, address.algorithm)

extension (sc: StringContext)
  def ci(args: Any*): CID[Encoded] = CID(sc.s(args*))
