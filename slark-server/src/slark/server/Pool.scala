package slark.server

import scala.collection.immutable.TreeMap

final class Pool[K: Ordering, V](allocate: K => V, release: V => Unit, alive: V => Boolean) {

  var map: TreeMap[K, V] = TreeMap.empty

  def apply(k: K): V = {
    map.get(k) match {
      case Some(v) if alive(v) => v
      case _ => {
        val tmp = allocate(k)
        map = map + (k -> tmp)
        tmp
      }
    }
  }

  def dry: this.type = {
    for ((k, v) <- map) {
      release(v)
    }
    this
  }
  
}