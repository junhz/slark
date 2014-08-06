package slark

object Readers {

  trait Linear {

    type T
    type Input = Reader

    trait Reader {
      def head: T
      def tail: Reader
      def atEnd: Boolean

      final def startWith(that: Reader): Option[Reader] = {
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

    def isSame(t1: T, t2: T): Boolean = t1.equals(t2)
  }

  trait Indexed {

    type Key
    type Value
    type Input = Reader

    trait Reader {
      def get(key: Key): Option[(Value, Reader)]
    }

  }

}