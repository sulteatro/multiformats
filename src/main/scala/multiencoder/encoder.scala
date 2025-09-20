package multiencoder

import encoding.Encoding

//
// Configure common encoders specified for multibase
//
object encoder:
  val pad: Option[Char] = Some('=')

  // lower bases: 2, 8, 10
  lazy val Base2: Encoding[2] = Encoding[2](alphabet.base2)
  lazy val Base8: Encoding[8] = Encoding[8](alphabet.base8)
  lazy val Base10: Encoding[10] = Encoding[10](alphabet.base10)

  // base 16 alphabet (lowercase & uppercase)
  lazy val Base16: Encoding[16] = Encoding[16](alphabet.base16)
  lazy val Base16Upper: Encoding[16] = Encoding[16](alphabet.base16.map(_.toUpper))

  // base 32 hex alphabet (lowercase & uppercase, with and without padding)
  lazy val Base32Hex: Encoding[32] = Encoding[32](alphabet.base32Hex)
  lazy val Base32HexUpper: Encoding[32] = Encoding[32](alphabet.base32Hex.map(_.toUpper))
  lazy val Base32HexPad: Encoding[32] = Encoding[32](alphabet.base32Hex, pad)
  lazy val Base32HexPadUpper: Encoding[32] = Encoding[32](alphabet.base32Hex.map(_.toUpper), pad)

  // base 32 standard alphabet (lowercase & uppercase, with and without padding)
  lazy val Base32: Encoding[32] = Encoding[32](alphabet.base32)
  lazy val Base32Upper: Encoding[32] = Encoding[32](alphabet.base32.map(_.toUpper))
  lazy val Base32Pad: Encoding[32] = Encoding[32](alphabet.base32, pad)
  lazy val Base32PadUpper: Encoding[32] = Encoding[32](alphabet.base32.map(_.toUpper), pad)

  // base 32 special alphabets (z-base-32, base32-sortable)
  lazy val Base32z: Encoding[32] = Encoding[32](alphabet.base32z)
  lazy val Base32Sortable: Encoding[32] = Encoding[32](alphabet.base32sortable, leftOriented = true)

  // base 36 (lowercase & uppercase)
  lazy val Base36: Encoding[36] = Encoding[36](alphabet.base36)
  lazy val Base36Upper: Encoding[36] = Encoding[36](alphabet.base36.map(_.toUpper))

  // base 58 (Bitcoin & Flickr alphabets)
  lazy val Base58BTC: Encoding[58] = Encoding[58](alphabet.base58btc)
  lazy val Base58Flickr: Encoding[58] = Encoding[58](alphabet.base58flickr)

  // base 64 (with and without padding, standard & url alphabets)
  lazy val Base64: Encoding[64] = Encoding[64](alphabet.base64)
  lazy val Base64Pad: Encoding[64] = Encoding[64](alphabet.base64, Some('='))
  lazy val Base64Url: Encoding[64] = Encoding[64](alphabet.base64url)
  lazy val Base64UrlPad: Encoding[64] = Encoding[64](alphabet.base64url, Some('='))
