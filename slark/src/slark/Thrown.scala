package slark

import scala.collection.mutable.ListBuffer

final class Thrown(ex: Throwable) {
  def fullMessage: List[String] = fullMessageAbove(0)
  def fullMessageAbove(ignoreFrames: Int): List[String] = {
    val buffer = new ListBuffer[String]
    var isOutMost = true
    val it = new Thrown.ExceptionChain(ex, ignoreFrames)
    while (it.hasNext) {
      val cnt = it.next
      buffer.append(if (isOutMost) cnt._1 else s"Caused by: ${cnt._1}")
      while (cnt._2.hasNext) {
        buffer.append(s"\tat ${cnt._2.next}")
      }
      isOutMost = false
    }
    buffer.toList
  }
}

object Thrown {

  class Trace(trace: Array[StackTraceElement], depth: Int) extends Iterator[String] {
    val limit = trace.length - depth
    var index = 0

    def hasNext = index < limit
    def next = if (hasNext) {
      val tmp = trace(index)
      index = index + 1
      tmp.toString()
    } else Iterator.empty.next
  }

  class ExceptionChain(ex: Throwable, initDepth: Int) extends Iterator[(String, Iterator[String])] {
    var cnt = ex
    var depth = initDepth

    def hasNext = cnt != null
    def next = if (hasNext) {
      val msg = cnt.toString()
      val trace = ex.getStackTrace()
      val tmp = (msg, new Trace(trace, depth))
      depth = trace.length
      cnt = cnt.getCause()
      tmp
    } else Iterator.empty.next
  }

}