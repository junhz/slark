package slark

object Readers {

  trait Linear[T] {

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

    val isSame: (T, T) => Boolean = (f1, f2) => f1.equals(f2)
  }

  trait Indexed[K, V] {

    type Input = Reader

    trait Reader {
      def get(key: K): Option[(V, Reader)]
    }

  }

}