package slark
package parser

trait ReaderApi {

  type From

  type Input <: Reader with ReaderOpt[Input]

  trait Reader {
    def head: From
    def tail: Reader
    def atEnd: Boolean
  }
  
  def isSame(f1: From, f2: From): Boolean = f1.equals(f2)

  trait ReaderOpt[R <: Reader] { self: R =>
    type Self = R with ReaderOpt[R]

    override def tail: Self

    def startWith(that: Self): Option[Self] = {
      @tailrec
      def rec(lhs: Self, rhs: Self): Option[Self] = {
        if (rhs.atEnd) Some(lhs)
        else if (lhs.atEnd) None
        else {
          log(info"lhs head: ${lhs.head}, rhs.head: ${rhs.head}")
          if (isSame(lhs.head, rhs.head)) {
            rec(lhs.tail, rhs.tail)
          } else None
        }
      }

      rec(this, that)
    }
  }
}