package slark

trait FailReason {

}

object FailReason {

  case class Exception[E <: Throwable](msg: Option[String], tpe: Class[E], stack: List[StackTraceElement]) extends FailReason {
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

  def causedBy(e: Throwable): List[FailReason] = {
    @tailrec
    def rec(e: Throwable, pre: List[FailReason], depth: Int): List[FailReason] = {
      val cnt = Exception(if (e.getMessage() == null) None else Some(e.getMessage()), e.getClass(), toList(e.getStackTrace(), depth))

      if (e.getCause() == e || e.getCause() == null) {
        (cnt :: pre).reverse
      } else rec(e.getCause(), cnt :: pre, e.getStackTrace().length)
    }
    rec(e, Nil, 0)
  }

  private[this] def toList(stack: Array[StackTraceElement], depth: Int): List[StackTraceElement] = {
    @tailrec
    def rec(off: Int, end: Int, pre: List[StackTraceElement]): List[StackTraceElement] = {
      if (off < end) {
        rec(off + 1, end, stack(off) :: pre)
      } else pre.reverse
    }
    rec(0, stack.length - depth, Nil)
  }

}