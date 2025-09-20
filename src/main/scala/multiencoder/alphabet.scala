package multiencoder

import array.SizedArray
import array.SizedArray.End

//
// Common alphabets used; individual encoder instances may vary by padding, casing, and other
// configuration details specific to their context.
//
object alphabet:
  //
  // Numeric bases: Base 2, Base 8, Base 10
  //
  lazy val base2: SizedArray[2, Char] =
    '0' :: '1' :: End

  lazy val base8: SizedArray[8, Char] =
    '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: End

  lazy val base10: SizedArray[10, Char] =
    '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: End

  //
  // Base 16 (hexadecimal alphabet)
  //
  lazy val base16: SizedArray[16, Char] =
    '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' ::
      '8' :: '9' :: 'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: End

  //
  // Base 32, extended hexadecimal alphabet
  //
  lazy val base32Hex: SizedArray[32, Char] =
    '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' ::
      'l' :: 'm' :: 'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: End

  //
  // Base 32 alphabet
  //
  lazy val base32: SizedArray[32, Char] =
    'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' ::
      'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' ::
      '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: End

  //
  // Base 32z (opinionated, human-oriented alphabet construction)
  //
  lazy val base32z: SizedArray[32, Char] =
    'y' :: 'b' :: 'n' :: 'd' :: 'r' :: 'f' :: 'g' :: '8' :: 'e' :: 'j' :: 'k' ::
      'm' :: 'c' :: 'p' :: 'q' :: 'x' :: 'o' :: 't' :: '1' :: 'u' :: 'w' :: 'i' ::
      's' :: 'z' :: 'a' :: '3' :: '4' :: '5' :: 'h' :: '7' :: '6' :: '9' :: End

  //
  // Base 32 sortable alphabet (used for TID encoding)
  //
  lazy val base32sortable: SizedArray[32, Char] =
    '2' :: '3' :: '4' :: '5' :: '6' :: '7' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' ::
      'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' :: End

  //
  // Base 36
  //
  lazy val base36: SizedArray[36, Char] =
    '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' ::
      'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' :: End

  //
  // Base 58 Bitcoin alphabet
  //
  lazy val base58btc: SizedArray[58, Char] =
    '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' ::
      'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'J' :: 'K' :: 'L' :: 'M' :: 'N' ::
      'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' :: 'W' :: 'X' :: 'Y' :: 'Z' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'm' :: 'n' ::
      'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' :: End

  //
  // Base 58 Flickr alphabet
  //
  lazy val base58flickr: SizedArray[58, Char] =
    '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'm' :: 'n' ::
      'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' ::
      'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'J' :: 'K' :: 'L' :: 'M' :: 'N' ::
      'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' :: 'W' :: 'X' :: 'Y' :: 'Z' :: End

  //
  // Base 64 standard alphabet
  //
  lazy val base64: SizedArray[64, Char] =
    'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'I' :: 'J' :: 'K' :: 'L' :: 'M' ::
      'N' :: 'O' :: 'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' :: 'W' :: 'X' :: 'Y' :: 'Z' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' ::
      'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' ::
      '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: '+' :: '/' :: End

  //
  // Base 64 URL alphabet
  //
  lazy val base64url: SizedArray[64, Char] =
    'A' :: 'B' :: 'C' :: 'D' :: 'E' :: 'F' :: 'G' :: 'H' :: 'I' :: 'J' :: 'K' :: 'L' :: 'M' ::
      'N' :: 'O' :: 'P' :: 'Q' :: 'R' :: 'S' :: 'T' :: 'U' :: 'V' :: 'W' :: 'X' :: 'Y' :: 'Z' ::
      'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: 'g' :: 'h' :: 'i' :: 'j' :: 'k' :: 'l' :: 'm' ::
      'n' :: 'o' :: 'p' :: 'q' :: 'r' :: 's' :: 't' :: 'u' :: 'v' :: 'w' :: 'x' :: 'y' :: 'z' ::
      '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: '-' :: '_' :: End
