package milletre.cache

import scala.collection.concurrent.TrieMap as ConcurrentMap

//
// HasCacheOps: typeclass for core caching functionality
//
trait HasCacheOps[C, K, V]:
  protected type Impl = C

  def get(impl: Impl, k: K): Option[V]
  def set(impl: Impl, k: K, v: V): V
  def pop(impl: Impl, k: K): Option[V]
  def clear(impl: Impl): Unit

object HasCacheOps:
  given [K, V] => HasCacheOps[ConcurrentMap[K, CacheEntry[V]], K, V]:
    override def get(impl: Impl, k: K): Option[V] =
      impl.get(k).map { e =>
        impl.put(k, e.access)
        e.value
      }

    override def set(impl: Impl, k: K, v: V): V =
      impl.put(k, impl.get(k).fold(CacheEntry(v))(_.update(v)))
      v

    override def pop(impl: Impl, k: K): Option[V] = impl.remove(k).map(_.value)
    override def clear(impl: Impl): Unit = impl.clear

  given [C <: Cache[?, K, V], K, V] => (ops: HasCacheOps[C, K, V]) => HasCacheOps[C, K, V]:
    override def get(impl: Impl, k: K): Option[V] = ops.get(impl, k)
    override def set(impl: Impl, k: K, v: V): V = ops.set(impl, k, v)
    override def pop(impl: Impl, k: K): Option[V] = ops.pop(impl, k)
    override def clear(impl: Impl): Unit = ops.clear(impl)

//
// Definition of the default in-memory implementation
//
type InMemory[K, V] = ConcurrentMap[K, CacheEntry[V]]
object InMemory:
  def empty[K, V] = ConcurrentMap.empty[K, CacheEntry[V]]

//
// Cache: Unbounded persistent cache with statistics
// all caches with eviction logic should extend this
//
trait Cache[C, K, V](using ops: HasCacheOps[C, K, V]):
  protected val impl: C

  def get(k: K): Option[V] = ops.get(impl, k)
  def set(k: K, v: V): V = ops.set(impl, k, v)
  def pop(k: K): Option[V] = ops.pop(impl, k)
  def clear: Unit = ops.clear(impl)

object Cache:
  def inMemory[K, V](using HasCacheOps[InMemory[K, V], K, V]): Cache[InMemory[K, V], K, V] =
    new Cache[InMemory[K, V], K, V]:
      val impl: InMemory[K, V] = ConcurrentMap.empty[K, CacheEntry[V]]
