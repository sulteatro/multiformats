package multiformats.cid

import milletre.constructor.*
import multiformats.multibase.Multibase
import multiformats.multibase.MultibaseAlgorithm
import multiformats.multibase.MultibaseAlgorithmFactory
import multiformats.multicodec.Multicodec
import multiformats.multicodec.MulticodecIngest
import multiformats.multicodec.MulticodecTag
import multiformats.multihash.Multihash
import multiformats.multihash.MultihashAlgorithm
import multiformats.multihash.MultihashAlgorithmFactory
import multiformats.varint.VarInt

val version: Multicodec = Multicodec.cidv1
val contentTags: Set[MulticodecTag] = Set(MulticodecTag.ipld)

//
// Implementation details:
// * parseCID: attempt to interpret a byte array as a content-type multicodec and multihash
// * buildCID: construct a CIDv1 from a content type multicodec and content address bytes
// * translateCID: encode a human-readable CID into its standard binary representation
//

private def parseCID(cid: Array[Byte]): Either[String, (Multicodec, Multihash)] =
  VarInt.sequence(cid, 2) match
    case (Array(cidCode, contentCode), mhBytes) =>
      for
        cidCodec <- Multicodec.validated(cidCode).filterOrElse(
          code => code.equals(version),
          s"Invalid CID multicodec code: '${cidCode.toHex}'"
        )
        contentCodec <- Multicodec.validated(contentCode).filterOrElse(
          code => contentTags.contains(code.tag),
          s"Invalid content-type multicodec code: '${contentCode.toHex}'"
        )
        contentAddress <- Multihash.validated(mhBytes)
      yield (contentCodec, contentAddress)
    case _ =>
      Left("Invalid CID format: could not extract Multicodec code varints")

private def buildCID(
    contentType: Multicodec,
    contentAddress: Multihash
): Either[String, Array[Byte]] =
  if contentTags.contains(contentType.tag) then
    Right(version.code.toBytes ++ contentType.code.toBytes ++ contentAddress.toBytes)
  else
    Left(s"Invalid content-type multicodec code: '${contentType.code.toHex}'")

private def translateCID(hr: String)(using
    MultibaseAlgorithmFactory[MultibaseAlgorithm]
): Either[String, Multibase] =
  hr.split(" - ") match
    case Array(baseName, cidCodec, typeName, address) if cidCodec.equals(version.toString) =>
      for
        contentType <- Multicodec.validated(typeName).filterOrElse(
          code => contentTags.contains(code.tag),
          s"Invalid content-type multicodec code: '${typeName}'"
        )
        contentAddress <- Multihash.validated(address)
        rawCID <- buildCID(contentType, contentAddress)
        baseAlgorithm <- MultibaseAlgorithm.getByName(baseName)
        encodedCID <- Multibase.encodeValidated(rawCID, baseAlgorithm)
      yield encodedCID
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
// Input validators: CIDIngest
//
trait CIDIngest[S, V] extends EitherConversion[V, CIDStateRepr[S]]

object CIDIngest:
  given CIDIngest[Raw, Array[Byte]] = v => parseCID(v).map(_ => v)

  given ingestMb: CIDIngest[Encoded, Multibase] = v => parseCID(v.decode).map(_ => v)
  given CIDIngest[Encoded, String] =
    v => if v.contains(" - ") then translateCID(v) else Multibase.validated(v).flatMap(ingestMb)

  given buildRawCID[T](using mci: MulticodecIngest[T]): CIDIngest[Raw, (Multihash, T)] =
    (address, typeSource) => mci(typeSource).flatMap(buildCID(_, address))

  given [T]
    => (MulticodecIngest[T])
    => CIDIngest[Raw, (Array[Byte], T)] =
    (address, typeSource) => Multihash.validated(address).flatMap(buildRawCID(_, typeSource))

  given buildEncodedCID[T, B](using
      mci: MulticodecIngest[T],
      mba: MultibaseAlgorithmFactory[B]
  ): CIDIngest[Encoded, (Multihash, T, B)] =
    (address, typeSource, baseSource) =>
      buildRawCID(address, typeSource).flatMap(Multibase.encodeValidated(_, baseSource))

  given [T, B]
    => (MulticodecIngest[T], MultibaseAlgorithmFactory[B])
    => CIDIngest[Encoded, (Array[Byte], T, B)] =
    (address, typeSource, baseSource) =>
      Multihash.validated(address).flatMap(buildEncodedCID(_, typeSource, baseSource))

//
// Input digestors: CIDDigest
//
trait CIDDigest[S, V] extends EitherConversion[V, CIDStateRepr[S]]

object CIDDigest:
  given createRawCID[T, H](using
      mci: MulticodecIngest[T],
      mhf: MultihashAlgorithmFactory[H]
  ): CIDDigest[Raw, (Array[Byte], T, H)] =
    (content, typeSource, hashSource) =>
      for
        hashAlgorithm <- mhf(hashSource)
        contentAddress <- Multihash.digestValidated(content, hashAlgorithm)
        contentType <- mci(typeSource)
        rawCID <- buildCID(contentType, contentAddress)
      yield rawCID

  given [T, H]
    => (MulticodecIngest[T], MultihashAlgorithmFactory[H])
    => CIDDigest[Raw, (String, T, H)] = (content, t, h) => createRawCID(content.getBytes, t, h)

  given [T, H]
    => (MulticodecIngest[T], MultihashAlgorithmFactory[H])
    => CIDDigest[Raw, (Multibase, T, H)] = (content, t, h) => createRawCID(content.decode, t, h)

  given createEncodedCID[T, H, B](
      using
      mci: MulticodecIngest[T],
      mhf: MultihashAlgorithmFactory[H],
      mbf: MultibaseAlgorithmFactory[B]
  ): CIDDigest[Encoded, (Array[Byte], T, H, B)] =
    (content, t, h, baseSource) =>
      createRawCID(content, t, h).flatMap(Multibase.encodeValidated(_, baseSource))

  given [T, H, B]
    => (MulticodecIngest[T], MultihashAlgorithmFactory[H], MultibaseAlgorithmFactory[B])
    => CIDDigest[Encoded, (String, T, H, B)] =
    (content, t, h, b) => createEncodedCID(content.getBytes, t, h, b)

  given [T, H, B]
    => (MulticodecIngest[T], MultihashAlgorithmFactory[H], MultibaseAlgorithmFactory[B])
    => CIDDigest[Encoded, (Multibase, T, H, B)] =
    (content, t, h, b) => createEncodedCID(content.decode, t, h, b)

//
// Public object interface: CID[Raw] and CID[Encoded]
//
opaque type CID[S] = CIDStateRepr[S]

object CID extends CaseMultiConstructor[CIDStateRepr, CID, CIDIngest]:

  //
  // Constructors that digest some provided content into an address with the given hash algorithm
  // then build a CID[Raw] if just content type is provided, or CID[Encoded] if a base algorithm
  // is also provided
  //
  def digestValidated[C, T, H](content: C, contentType: T, hash: H)(using
      mcf: MulticodecIngest[T],
      maf: MultihashAlgorithmFactory[H],
      c: CIDDigest[Raw, (C, T, H)]
  ): Either[String, CID[Raw]] = c(content, contentType, hash)

  def digestIfValid[C, T, H](content: C, contentType: T, hash: H)(using
      mcf: MulticodecIngest[T],
      maf: MultihashAlgorithmFactory[H],
      c: CIDDigest[Raw, (C, T, H)]
  ): Option[CID[Raw]] = c(content, contentType, hash).toOption

  def digest[C, T, H](content: C, contentType: T, hash: H)(using
      mcf: MulticodecIngest[T],
      maf: MultihashAlgorithmFactory[H],
      c: CIDDigest[Raw, (C, T, H)]
  ): CID[Raw] =
    c(content, contentType, hash).fold(error => throw ValidationError[CID[Raw]](error), identity)

  def digestValidated[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
      mci: MulticodecIngest[T],
      mhf: MultihashAlgorithmFactory[H],
      mbf: MultibaseAlgorithmFactory[B],
      c: CIDDigest[Encoded, (C, T, H, B)]
  ): Either[String, CID[Encoded]] = c(content, contentType, hash, base)

  def digestIfValid[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
      mci: MulticodecIngest[T],
      mhf: MultihashAlgorithmFactory[H],
      mbf: MultibaseAlgorithmFactory[B],
      c: CIDDigest[Encoded, (C, T, H, B)]
  ): Option[CID[Encoded]] = c(content, contentType, hash, base).toOption

  def digest[C, T, H, B](content: C, contentType: T, hash: H, base: B)(using
      mci: MulticodecIngest[T],
      mhf: MultihashAlgorithmFactory[H],
      mbf: MultibaseAlgorithmFactory[B],
      c: CIDDigest[Encoded, (C, T, H, B)]
  ): CID[Encoded] =
    c(content, contentType, hash, base).fold(
      error => throw ValidationError[CID[Encoded]](error),
      identity
    )

  extension (cidRaw: CID[Raw])
    def toBytes: Array[Byte] = cidRaw

    def encode[B](base: B)(using MultibaseAlgorithmFactory[B]): CID[Encoded] =
      Multibase.encode(toBytes, base)

    def =~(other: CID[Raw]): Boolean = toBytes.toSeq.equals(other.toBytes.toSeq)
    def =~(other: CID[Encoded]): Boolean = cidRaw =~ other.decode
    def !~(other: CID[Raw]): Boolean = !(cidRaw =~ other)
    def !~(other: CID[Encoded]): Boolean = !(cidRaw =~ other)

    def codec: Multicodec = version
    def contentType: Multicodec = parseCID(cidRaw).map(_.head).toOption.get
    def address: Multihash = parseCID(cidRaw).map(_.last).toOption.get

    def isAddressOf[C](content: C)(using
        MulticodecIngest[Multicodec],
        MultihashAlgorithmFactory[MultihashAlgorithm],
        CIDDigest[Raw, (C, Multicodec, MultihashAlgorithm)]
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
    def encoding: MultibaseAlgorithm = MultibaseAlgorithm.getByChar(cidEncoded.prefix).toOption.get

    def toHumanReadable: String =
      Vector(
        encoding.toString,
        version.toString,
        contentType.toString,
        address.toHumanReadable
      ).mkString(" - ")

    def isAddressOf[C](content: C)(using
        MulticodecIngest[Multicodec],
        MultihashAlgorithmFactory[MultihashAlgorithm],
        CIDDigest[Raw, (C, Multicodec, MultihashAlgorithm)]
    ): Boolean = cidEncoded =~ CID.digest(content, contentType, address.algorithm)

extension (sc: StringContext)
  def ci(args: Any*): CID[Encoded] = CID(sc.s(args*))
