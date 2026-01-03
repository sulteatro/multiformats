package milletre.cache

//
// HasEvictOps: typeclass for evicting entries when the cache exceeds a maximum size
//
sealed trait EvictionAlgorithm

// Next, define a typeclass for each algorithm providing the sort key from a CacheEntry object
sealed trait EvictionOrdering extends EvictionAlgorithm:
  val maxSize: Int
  def compare(l: CacheEntry[?], r: CacheEntry[?]): Boolean

object EvictionAlgorithm:
  import CacheEntry.*
  case object UNBOUND extends EvictionAlgorithm

  case class LRU(override val maxSize: Int) extends EvictionAlgorithm with EvictionOrdering:
    def compare(l: CacheEntry[?], r: CacheEntry[?]): Boolean = l.lastAccess < r.lastAccess

  case class LFU(override val maxSize: Int) extends EvictionAlgorithm with EvictionOrdering:
    def compare(l: CacheEntry[?], r: CacheEntry[?]): Boolean =
      l.accessCount < r.accessCount || (l.accessCount == r.accessCount && l.lastAccess < r.lastAccess)

  case class FIFO(override val maxSize: Int) extends EvictionAlgorithm with EvictionOrdering:
    def compare(l: CacheEntry[?], r: CacheEntry[?]): Boolean = l.createdAt < r.createdAt

// Finally, define the HasEvictOps operations typeclass
trait HasEvictOps[A <: EvictionAlgorithm, C, K, V]:
  protected type Impl = C

  def evict(impl: Impl, algo: A): C

object HasEvictOps:
  import EvictionAlgorithm.UNBOUND

  given [M, K, V] => HasEvictOps[UNBOUND.type, M, K, V]:
    def evict(impl: Impl, algo: UNBOUND.type): M = ???

  given [A <: EvictionOrdering, K, V] => HasEvictOps[A, InMemory[K, V], K, V]:
    def evict(impl: Impl, algo: A): InMemory[K, V] =
      impl.toSeq
        .sortWith { case ((_, l), (_, r)) => algo.compare(l, r) }
        .dropRight(algo.maxSize)
        .map(_.head)
        .foldLeft(InMemory.empty[K, V]) {
          (out, k) => impl.remove(k).fold(out)(out.addOne(k, _))
        }

//
// SizedCache: Persistent cache with a maximum size
//
trait SizedCache[A <: EvictionAlgorithm, C, K, V](val evictionAlgorithm: A)(using
    coreOps: HasCacheOps[C, K, V],
    sizeOps: HasEvictOps[A, C, K, V]
):
  this: Cache[C, K, V] =>
  override def prune: Unit = sizeOps.evict(impl, evictionAlgorithm)

  override def set(k: K, v: V): V =
    coreOps.set(impl, k, v)
    prune
    v
