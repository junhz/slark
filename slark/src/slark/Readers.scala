package slark

trait Readers[F] { self: AbstractInput =>

  type From = F

  type Input = Reader

  trait Reader {
    def head: From
    def tail: Reader
    def atEnd: Boolean

    def startWith(that: Reader): Option[Reader] = {
      @tailrec
      def rec(lhs: Reader, rhs: Reader): Option[Reader] = {
        if (rhs.atEnd) Some(lhs)
        else if (lhs.atEnd) None
        else {
          if (isSame(lhs.head, rhs.head)) {
            rec(lhs.tail, rhs.tail)
          } else None
        }
      }

      rec(this, that)
    }
  }

  val isSame: (From, From) => Boolean = (f1, f2) => f1.equals(f2)
}