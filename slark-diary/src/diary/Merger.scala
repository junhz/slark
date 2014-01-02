package slark
package diary

import scala.collection.immutable.Traversable
import Diff._

trait Merger[T] {
  def apply(scripts: Scripts, source: Traversable[T], modified: Traversable[T]): List[T]
}

object Merger {
  object SimpleAutoMerger extends Merger[(String, String)] {
    def apply(scripts: Scripts, source: Traversable[(String, String)], modified: Traversable[(String, String)]): List[(String, String)] = {
      @tailrec
      def rec(scripts: Scripts, source: Traversable[(String, String)], modified: Traversable[(String, String)], merged: List[(String, String)]): List[(String, String)] = {
        if (scripts.isEmpty) merged.reverse
        else {
          scripts.head match {
            case Insert => {
              val (in, out) = modified.head
              if (conform(s"for source \r\n$in\r\nis output \r\n$out\r\nexpected?")) rec(scripts.tail, source, modified.tail, (in, out) :: merged)
              else {
                println("output not expected will not be saved")
                rec(scripts.tail, source, modified.tail, merged)
              }
            }
            case Remain => {
              if (source.head._2 equals modified.head._2) rec(scripts.tail, source.tail, modified.tail, source.head :: merged)
              else {
                if (conform(s"for source \r\n${source.head._1}\r\nis output changed from \r\n${source.head._2}\r\nto\r\n${modified.head._2}\r\nexpected?")) {
                  rec(scripts.tail, source.tail, modified.tail, modified.head :: merged)
                } else {
                  println("original output is saved.")
                  rec(scripts.tail, source.tail, modified.tail, source.head :: merged)
                }
              }
            }
            case Delete => rec(scripts.tail, source.tail, modified, merged)
          }
        }
      }

      rec(scripts, source, modified, Nil)
    }
  }

  def conform(msg: String): Boolean = {
    println(msg)
    readBoolean
  }

}