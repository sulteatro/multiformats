package multiformats.multihash

import org.bouncycastle.crypto.digests.Blake3Digest
import org.bouncycastle.jce.provider.BouncyCastleProvider

import java.security.MessageDigest
import java.security.MessageDigestSpi
import java.security.Provider
import java.security.Security

private[multihash] object security:
  class Blake3Provider extends Provider("Blake3Provider", "1.0.0", "BLAKE3 MessageDigest Provider"):
    private class Blake3DigestSpi extends MessageDigestSpi:
      private val digest: Blake3Digest = new Blake3Digest
      private val singleByte: Array[Byte] = Array(0.toByte)

      override protected def engineUpdate(input: Byte): Unit =
        singleByte(0) = input
        digest.update(singleByte, 0, 1);

      override protected def engineUpdate(input: Array[Byte], offset: Int, len: Int): Unit =
        digest.update(input, offset, len);

      override protected def engineDigest: Array[Byte] =
        val output = Array.fill[Byte](digest.getDigestSize())(0)
        digest.doFinal(output, 0)
        output

      override protected def engineReset: Unit = digest.reset

      override protected def engineGetDigestLength: Int = digest.getDigestSize

    put("MessageDigest.BLAKE3", classOf[Blake3DigestSpi].getName)
    put("MessageDigest.blake3", classOf[Blake3DigestSpi].getName) // allow lowercase alias

  def size(name: String): Int = MessageDigest.getInstance(name).getDigestLength

  def digest(name: String, barr: Array[Byte]): Either[String, Array[Byte]] =
    val messageDigest = MessageDigest.getInstance(name)
    messageDigest.update(barr)
    val (hash, size) = (messageDigest.digest(), messageDigest.getDigestLength)
    if hash.length.equals(size) then
      Right(hash)
    else
      Left(s"Mismatch between expected and realized digest sizes: $size vs ${hash.length}")

  def addMultihashProviders(): Unit =
    // Add extra crypto algorithms provided by Bouncy Castle
    Security.addProvider(new BouncyCastleProvider)
    // Add custom Blake3Provider using Bouncy Castle's raw Blake3Digest algorithm
    Security.addProvider(new Blake3Provider)
