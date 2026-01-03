package milletre.cache

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

//
// Cache entry including statistics used by eviction logic (such as age, order, etc.)
//

case class CacheEntry[V](
    private[cache] value: V,
    createdAt: CacheEntry.Timestamp,
    lastUpdate: CacheEntry.Timestamp,
    lastAccess: CacheEntry.Timestamp,
    updateCount: Int = 0,
    accessCount: Int = 0,
    expiresAt: Option[CacheEntry.Timestamp] = None
)

object CacheEntry:
  type Timestamp = Instant
  type Timedelta = FiniteDuration

  object Timestamp:
    def now: Timestamp = Instant.now

  extension (ts: Timestamp)
    def <(other: Timestamp): Boolean = other.isAfter(ts)
    def >(other: Timestamp): Boolean = ts.isAfter(other)
    def +(ttl: Timedelta): Timestamp = ts.plusSeconds(ttl.toSeconds)

  extension [V](e: CacheEntry[V])
    def isValid: Boolean = !e.expiresAt.exists(_ < Timestamp.now)
    def get: Option[V] = e.expiresAt.filterNot(_ < Timestamp.now).fold(Some(e.value))(_ => None)

    def update(v: V): CacheEntry[V] =
      e.copy(value = v, lastUpdate = Timestamp.now, updateCount = e.updateCount + 1)
    def access: CacheEntry[V] = e.copy(lastAccess = Timestamp.now, accessCount = e.accessCount + 1)

    def persist: CacheEntry[V] = e.copy(lastUpdate = Timestamp.now, expiresAt = None)
    def extend(ttl: FiniteDuration): CacheEntry[V] =
      val ts = Timestamp.now
      e.copy(lastUpdate = ts, expiresAt = Some(ts + ttl))

  def apply[V](v: V): CacheEntry[V] =
    val ts = Timestamp.now
    CacheEntry(value = v, createdAt = ts, lastUpdate = ts, lastAccess = ts)

  def apply[V](v: V, ttl: FiniteDuration): CacheEntry[V] =
    val ts = Timestamp.now
    CacheEntry(
      value = v,
      createdAt = ts,
      lastUpdate = ts,
      lastAccess = ts,
      expiresAt = Some(ts + ttl)
    )
