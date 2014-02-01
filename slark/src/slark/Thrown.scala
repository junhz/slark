package slark

import scala.collection.mutable.ListBuffer

case class Thrown[E <: Throwable](msg: Option[String], tpe: Class[E], stack: List[StackTraceElement]) {
  override def toString = {
    val message = msg match {
      case None => tpe.getName()
      case Some(s) => s"${tpe.getName()}: $s"
    }
    stack match {
      case Nil => message
      case _ => s"$message\r\n${stack.mkString("\tat ", "\r\n\tat ", "")}"
    }
  }
}
object Thrown {
  def cnt(): List[Thrown[_]] = wrap(new Exception)
  def wrap[E <: Throwable](e: E): List[Thrown[_]] = {
    @tailrec
    def rec(e: Throwable, pre: List[Thrown[_]], depth: Int): List[Thrown[_]] = {
      val cnt = Thrown(if(e.getMessage() == null) None else Some(e.getMessage()), e.getClass(), toList(e.getStackTrace(), depth))
      
      if(e.getCause() == e || e.getCause() == null) {
        (cnt :: pre).reverse
      } else rec(e.getCause(), cnt :: pre, e.getStackTrace().length)
    }
    rec(e, Nil, 0)
  }
  private[this] def toList(stack: Array[StackTraceElement], depth: Int): List[StackTraceElement] = {
    @tailrec
    def rec(off: Int, end: Int, pre: List[StackTraceElement]): List[StackTraceElement] = {
      if(off < end) {
        rec(off + 1, end, stack(off) :: pre)
      } else pre.reverse
    }
    rec(0, stack.length - depth, Nil)
  }
}