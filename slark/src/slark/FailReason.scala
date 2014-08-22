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
    
    def toList[T](array: Array[T], dropRight: Int): List[T] = {
      @tailrec
      def rec(off: Int, end: Int, pre: List[T]): List[T] = {
        if (off < end) {
          rec(off + 1, end, array(off) :: pre)
        } else pre.reverse
      }
      rec(0, array.length - dropRight, Nil)
    }
    
    @tailrec
    def rec(e: Throwable, pre: List[FailReason], depth: Int): List[FailReason] = {
      val cnt = Exception(if (e.getMessage() == null) None else Some(e.getMessage()), e.getClass(), toList(e.getStackTrace(), depth))

      if (e.getCause() == e || e.getCause() == null) {
        (cnt :: pre).reverse
      } else rec(e.getCause(), cnt :: pre, e.getStackTrace().length)
    }
    rec(e, Nil, 0)
  }
  
  def apply(msg: String): FailReason = new FailReason {
    override def toString = msg
  }
  
}