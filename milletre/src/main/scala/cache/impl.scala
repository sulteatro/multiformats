package milletre.cache

import scala.collection.concurrent.TrieMap as ConcurrentMap

import CacheEntry.{Timestamp, Timedelta}

//
// SizedTTLCache: implementation-flexible cache gluing together HasCacheOps and HasTTLOps
//
trait SizedTTLCache[A <: EvictionAlgorithm, C, K, V](
    val evictionAlgorithm: A,
    val defaultTTL: Option[Timedelta]
)(using
    coreOps: HasCacheOps[C, K, V],
    sizeOps: HasEvictOps[A, C, K, V],
    ttlOps: HasTTLOps[C, K, V]
):
  this: Cache[C, K, V] =>

  override def set(k: K, v: V): V =
    defaultTTL.fold(coreOps.set(impl, k, v))(ttl => ttlOps.set(impl, k, v, ttl))
    sizeOps.evict(impl, evictionAlgorithm)
    v

  override def set(k: K, v: V, ttl: Timedelta): V =
    ttlOps.set(impl, k, v, ttl)
    sizeOps.evict(impl, evictionAlgorithm)
    v

  override def ttl(k: K, ttl: Timedelta): Option[Timestamp] = ttlOps.ttl(impl, k, ttl)

  override def prune: Unit =
    sizeOps.evict(impl, evictionAlgorithm)
    ttlOps.prune(impl)

//
// Definition of the default in-memory implementation
//
type InMemory[K, V] = ConcurrentMap[K, CacheEntry[V]]
object InMemory:
  def empty[K, V] = ConcurrentMap.empty[K, CacheEntry[V]]

object CacheBuilder:
  type BuilderFn =
    [M, K, V] =>> [A <: EvictionAlgorithm] => (A, Option[Option[Timedelta]]) => (
        HasCacheOps[M, K, V],
        HasEvictOps[A, M, K, V],
        HasTTLOps[M, K, V]
    ) ?=> Cache[M, K, V]

  private def build[M, K, V](impl: M): BuilderFn[M, K, V] =
    [A <: EvictionAlgorithm] =>
      (
          evictionAlgorithm: A,
          withDefaultTTL: Option[Option[Timedelta]]
      ) =>
        (
            c: HasCacheOps[M, K, V],
            e: HasEvictOps[A, M, K, V],
            t: HasTTLOps[M, K, V]
        ) ?=>
          (evictionAlgorithm, withDefaultTTL) match
            case (evictionAlgorithm: EvictionOrdering, Some(defaultTTL)) =>
              new Cache[M, K, V](impl)
                with SizedTTLCache[A, M, K, V](evictionAlgorithm, defaultTTL) {}
            case (evictionAlgorithm: EvictionOrdering, None) =>
              new Cache[M, K, V](impl) with SizedCache[A, M, K, V](evictionAlgorithm) {}
            case (_, Some(defaultTTL)) =>
              new Cache[M, K, V](impl) with TTLCache[M, K, V](defaultTTL) {}
            case (_, None) =>
              new Cache[M, K, V](impl) {}

  def inMemory[K, V]: BuilderFn[InMemory[K, V], K, V] =
    build[InMemory[K, V], K, V](InMemory.empty[K, V])
