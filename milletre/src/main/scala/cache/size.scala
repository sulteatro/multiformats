package milletre.cache

//
// HasEvictOps: typeclass for evicting entries when the cache exceeds a maximum size
//
sealed trait EvictionAlgorithm
trait LRU extends EvictionAlgorithm
trait LFU extends EvictionAlgorithm
trait FIFO extends EvictionAlgorithm

// Next, define a typeclass for each algorithm providing the sort key from a CacheEntry object
trait EvictionOrdering[A <: EvictionAlgorithm]:
  def apply(l: CacheEntry[?], r: CacheEntry[?]): Boolean

object EvictionOrdering:
  import CacheEntry.*

  given lru: EvictionOrdering[LRU] = (l, r) => l.lastAccess < r.lastAccess
  given EvictionOrdering[FIFO] = (l, r) => l.createdAt < r.createdAt
  given EvictionOrdering[LFU] =
    (l, r) =>
      l.accessCount < r.accessCount || (l.accessCount == r.accessCount && lru(l, r))

// Finally, define the HasEvictOps operations typeclass
trait HasEvictOps[A <: EvictionAlgorithm, C, K, V]:
  protected type Impl = C

  def evict(impl: Impl, size: Int): C

object HasEvictOps:
  given [A <: EvictionAlgorithm, K, V]
    => (ordering: EvictionOrdering[A])
      => HasEvictOps[A, InMemory[K, V], K, V]:
    def evict(impl: Impl, size: Int): InMemory[K, V] =
      impl.toSeq
        .sortWith { case ((_, l), (_, r)) => ordering(l, r) }
        .dropRight(size)
        .map(_.head)
        .foldLeft(InMemory.empty[K, V]) {
          (out, k) => impl.remove(k).fold(out)(out.addOne(k, _))
        }

//
// SizedCache: Persistent cache with a maximum size
//
trait SizedCache[A <: EvictionAlgorithm, C, K, V](using
    coreOps: HasCacheOps[C, K, V],
    sizeOps: HasEvictOps[A, C, K, V]
) extends Cache[C, K, V]:
  val maxSize: Int

  override def set(k: K, v: V): V =
    coreOps.set(impl, k, v)
    sizeOps.evict(impl, maxSize)
    v

object SizedCache:
  def inMemory[A <: EvictionAlgorithm, K, V](maxSize: Int)(using
      HasCacheOps[InMemory[K, V], K, V],
      HasEvictOps[A, InMemory[K, V], K, V]
  ): SizedCache[A, InMemory[K, V], K, V] =
    new SizedCache[A, InMemory[K, V], K, V]:
      protected val impl: InMemory[K, V] = InMemory.empty[K, V]
      val maxSize: Int = maxSize
