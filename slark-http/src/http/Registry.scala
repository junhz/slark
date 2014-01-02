package junhz
package http

class Registry[K, V](cache: Map[K, V], fn: V => K) {

  def apply(key: K, default: V): V = cache.getOrElse(key, default)

  def +(value: V): Registry[K, V] = new Registry(cache + ((fn(value), value)), fn)

}

object Registry {
  def apply[K, V](fn: V => K): Registry[K, V] = new Registry(Map.empty, fn)
}