package milletre.cache

import CacheEntry.{Timestamp, Timedelta}

//
// HasCacheTTLOps: typeclass for core caching functionality
//
trait HasTTLOps[C, K, V]:
  protected type Impl = C

  def set(impl: Impl, k: K, v: V, ttl: Timedelta): V
  def ttl(impl: Impl, k: K, ttl: Timedelta): Option[Timestamp]
  def pin(impl: Impl, k: K): Option[Timestamp]
  def prune(impl: Impl): Unit

object HasTTLOps:
  given [K, V] => HasTTLOps[InMemory[K, V], K, V]:
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
// TTLCache: implementation-flexible cache gluing together HasCacheOps and HasTTLOps
//
trait TTLCache[C, K, V](
    val defaultTTL: Option[Timedelta]
)(using
    coreOps: HasCacheOps[C, K, V],
    ttlOps: HasTTLOps[C, K, V]
):
  this: Cache[C, K, V] =>
  override def set(k: K, v: V): V =
    defaultTTL.fold(coreOps.set(impl, k, v))(ttl => ttlOps.set(impl, k, v, ttl))

  override def set(k: K, v: V, ttl: Timedelta): V = ttlOps.set(impl, k, v, ttl)
  override def ttl(k: K, ttl: Timedelta): Option[Timestamp] = ttlOps.ttl(impl, k, ttl)
  override def prune: Unit = ttlOps.prune(impl)
