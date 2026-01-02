package milletre.cache

import CacheEntry.{Timestamp, Timedelta}

//
// HasCacheTTLOps: typeclass for core caching functionality
//
trait HasTTLCacheOps[C, K, V]:
  protected type Impl = C

  def set(impl: Impl, k: K, v: V, ttl: Timedelta): V
  def ttl(impl: Impl, k: K, ttl: Timedelta): Option[Timestamp]
  def pin(impl: Impl, k: K): Option[Timestamp]
  def prune(impl: Impl): Unit

object HasTTLCacheOps:
  given [K, V] => HasTTLCacheOps[InMemory[K, V], K, V]:
    override def set(impl: Impl, k: K, v: V, ttl: Timedelta): V =
      impl.put(k, CacheEntry(v, ttl))
      v

    override def ttl(impl: Impl, k: K, ttl: Timedelta): Option[Timestamp] =
      impl.get(k).map(_.extend(ttl)).flatMap { e =>
        impl.put(k, e)
        e.expiresAt
      }

    override def pin(impl: Impl, k: K): Option[Timestamp] =
      impl.get(k).map(_.persist).flatMap(impl.put(k, _)).flatMap(_.expiresAt)

    override def prune(impl: Impl): Unit = impl.filterInPlace((_, e) => e.isValid)

//
// TTLCache: implementation-flexible cache gluing together HasCacheOps and HasTTLCacheOps
//
trait TTLCache[A <: EvictionAlgorithm, C, K, V](
)(using
    coreOps: HasCacheOps[C, K, V],
    sizeOps: HasEvictOps[A, C, K, V],
    ttlOps: HasTTLCacheOps[C, K, V]
) extends SizedCache[A, C, K, V]:
  val defaultTTL: Option[Timedelta]

  override def set(k: K, v: V): V =
    defaultTTL.fold(coreOps.set(impl, k, v))(ttl => ttlOps.set(impl, k, v, ttl))
    sizeOps.evict(impl, maxSize)
    v

  def set(k: K, v: V, ttl: Timedelta): V =
    ttlOps.set(impl, k, v, ttl)
    sizeOps.evict(impl, maxSize)
    v

  def ttl(k: K, ttl: Timedelta): Option[Timestamp] = ttlOps.ttl(impl, k, ttl)
  def prune: Unit = ttlOps.prune(impl)

object TTLCache:
  def inMemory[A <: EvictionAlgorithm, K, V](
      maxSize: Int,
      defaultTTL: Option[Timedelta] = None
  )(using
      HasCacheOps[InMemory[K, V], K, V],
      HasEvictOps[A, InMemory[K, V], K, V],
      HasTTLCacheOps[InMemory[K, V], K, V]
  ): TTLCache[A, InMemory[K, V], K, V] =
    new TTLCache[A, InMemory[K, V], K, V]:
      protected val impl: InMemory[K, V] = InMemory.empty[K, V]
      val maxSize: Int = maxSize
      val defaultTTL: Option[Timedelta] = defaultTTL
