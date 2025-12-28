package multiformats

import cid.{CID, Encoded}
import multibase.Multibase
import multicodec.Multicodec
import multihash.Multihash

object string:
  extension (sc: StringContext)
    def mb(args: Any*): Multibase = Multibase(sc.s(args*))
    def mc(args: Any*): Multicodec = Multicodec(sc.s(args*))
    def mh(args: Any*): Multihash = Multihash(sc.s(args*))
    def cid(args: Any*): CID[Encoded] = CID(sc.s(args*))
