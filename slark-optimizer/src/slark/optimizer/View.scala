package slark.optimizer

/**
 * @author a554114
 */
object View {
  
  trait Travesal[A] { self =>
    def iterator: Iterator[A]
    
    final def foreach[U](f: A => U): Unit = {
      val it = iterator
      while (it.hasNext) {
        f(it.next())
      }
    }
    final def map[B](f: A => B): Travesal[B] = TravesalMapped(self, f)
    final def max(implicit ord: Ordering[A]): Option[A] = {
      val it = iterator
      if (it.hasNext) {
        var max = it.next()
        while (it.hasNext) {
          val cnt = it.next()
          if (ord.compare(cnt, max) > 0) max = cnt
          else ()
        }
        Some(max)
      } else None
    }
    final def min(implicit ord: Ordering[A]): Option[A] = {
      val it = iterator
      if (it.hasNext) {
        var min = it.next()
        while (it.hasNext) {
          val cnt = it.next()
          if (ord.compare(cnt, min) < 0) min = cnt
          else ()
        }
        Some(min)
      } else None
    }
    final def maxBy[B <: Ordered[B]](f: A => B): Option[A] = {
      val it = iterator
      if (it.hasNext) {
        var max = it.next()
        var maxOrdered = f(max)
        while (it.hasNext) {
          val cnt = it.next()
          val cntOrdered = f(cnt)
          if (cntOrdered > maxOrdered) {
            maxOrdered = cntOrdered
            max = cnt
          } else ()
        }
        Some(max)
      }
      else None
    }
    final def minBy[B <: Ordered[B]](f: A => B): Option[A] = {
      val it = iterator
      if (it.hasNext) {
        var min = it.next()
        var minOrdered = f(min)
        while (it.hasNext) {
          val cnt = it.next()
          val cntOrdered = f(cnt)
          if (cntOrdered < minOrdered) {
            minOrdered = cntOrdered
            min = cnt
          } else ()
        }
        Some(min)
      }
      else None
    }
    final def some(cond: A => Boolean): Travesal[A] = TravesalTravesal(self, cond)
  }
  
  case class TravesalMapped[A, B](underlying: Travesal[A], mapping: A => B) extends Travesal[B] {
    def iterator = {
      val it = underlying.iterator
      new Iterator[B] {
        def hasNext = it.hasNext
        def next = mapping(it.next())
      }
    }
  }
  
  case class TravesalTravesal[A](underlying: Travesal[A], cond: A => Boolean) extends Travesal[A] {
    def iterator = {
      val it = underlying.iterator
      new Iterator[A] {
        var last: Option[A] = None
        def hasNext = !last.isEmpty || fill
        def next = {
          val cnt = last.get
          last = None
          cnt
        }
        def fill = {
          var filled = false
          while (!filled && it.hasNext) {
            val cnt = it.next()
            if (cond(cnt)) {
              filled = true
              last = Some(cnt)
            } else ()
          }
          filled
        }
      }
    }
  }
  
  trait Indexed[A] { self =>
    def length: Int
    def apply(idx: Int): A
    final def foreach[U](f: A => U): Unit = {
      var idx = 0
      val len = length
      while (idx < len) {
        f(apply(idx))
        idx += 1
      }
    }
    
    final def indexed = IndexedIndexed(self)
    final def range(from: Int, to: Int/*excluded*/) = IndexedRanged(self, from, to)
    final def toArray(implicit tag: reflect.ClassTag[A]): scala.Array[A] = {
      val len = length
      val arr = tag.newArray(len)
      var idx = 0
      while (idx < len) {
        arr(idx) = apply(idx)
        idx += 1
      }
      arr
    }
    final def max(implicit ord: Ordering[A]): A = {
      val len = length
      if (len == 0) throw new NoSuchElementException("max of empty")
      else {
        var max = apply(0)
        var idx = 1
        while (idx < len) {
          val cnt = apply(idx)
          if (ord.compare(cnt, max) > 0) max = cnt
          else ()
          idx += 1
        }
        max
      }
    }
    final def min(implicit ord: Ordering[A]): A = {
      val len = length
      if (len == 0) throw new NoSuchElementException("min of empty")
      else {
        var min = apply(0)
        var idx = 1
        while (idx < len) {
          val cnt = apply(idx)
          if (ord.compare(cnt, min) < 0) min = cnt
          else ()
          idx += 1
        }
        min
      }
    }
    final def maxBy[B <: Ordered[B]](f: A => B): A = {
      val len = length
      if (len == 0) throw new NoSuchElementException("maxBy of empty")
      else {
        var max = apply(0)
        var maxOrdered = f(max)
        var idx = 1
        while (idx < len) {
          val cnt = apply(idx)
          val cntOrdered = f(cnt)
          if (cntOrdered > maxOrdered) {
            maxOrdered = cntOrdered
            max = cnt
          } else ()
          idx += 1
        }
        max
      }
    }
    final def minBy[B <: Ordered[B]](f: A => B): A = {
      val len = length
      if (len == 0) throw new NoSuchElementException("minBy of empty")
      else {
        var min = apply(0)
        var minOrdered = f(min)
        var idx = 1
        while (idx < len) {
          val cnt = apply(idx)
          val cntOrdered = f(cnt)
          if (cntOrdered < minOrdered) {
            minOrdered = cntOrdered
            min = cnt
          } else ()
          idx += 1
        }
        min
      }
    }
    final def mkString: String = mkString("")
    final def mkString(sep: String): String = {
      val len = length
      if (len == 0) ""
      else {
        val sb = new StringBuilder
        sb.append(apply(0))
        var idx = 1
        while (idx < len) {
          sb.append(sep)
          sb.append(apply(idx))
          idx += 1
        }
        sb.toString
      }
    }
    final def forall[U](f: A => Boolean): Boolean = {
      val len = length
      var idx = 0
      var r = true
      while (idx < len) {
        r &= f(apply(idx))
        idx += 1
      }
      r
    }
    final def map[B](f: A => B): Indexed[B] = IndexedMapped(self, f)
    final def some(condition: A => Boolean): Travesal[A] = new Travesal[A] {
      def iterator = new Iterator[A] {
        var last: Option[A] = None
        var cnt = 0
        var len = self.length
        def fill: Boolean = {
          var filled = false
          while (!filled && cnt < len) {
            val a = self.apply(cnt)
            if (condition(a)) {
              last = Some(a)
              filled = true
            }
            cnt += 1
          }
          filled
        }
        def hasNext = !last.isEmpty || fill
        def next = {
          val r = last.get
          last = None
          r
        }
      }
    }
  }
  
  case class Array[A](array: scala.Array[A]) extends Indexed[A] {
    def length = array.length
    def apply(idx: Int) = array(idx)
  }
  
  case class IndexedMapped[A, B](underlying: Indexed[A], mapping: A => B) extends Indexed[B] {
    def length = underlying.length
    def apply(idx: Int) = mapping(underlying(idx))
  }
  
  case class IndexedIndexed[A](underlying: Indexed[A]) extends Indexed[(Int, A)] {
    def length = underlying.length
    def apply(idx: Int) = (idx, underlying(idx))
  }
  
  case class IndexedRanged[A](underlying: Indexed[A], from: Int, to: Int/*excluded*/) extends Indexed[A] {
    def length = to - from
    def apply(idx: Int) = underlying(idx + from)
  }
  
  case class Cols[A](array: scala.Array[scala.Array[A]]) extends Indexed[Indexed[A]] {
    def apply(col: Int) = new Indexed[A] {
      def apply(row: Int) = array(row)(col)
      def length: Int = array.length
    }
    val length: Int = Array(array).map(_.length).max
  }
  case class Rows[A](array: scala.Array[scala.Array[A]]) extends Indexed[Indexed[A]] {
    val colLen = Array(array).map(_.length).max
    def apply(row: Int) = new Indexed[A] {
      def apply(col: Int) = array(row)(col)
      def length: Int = colLen
    }
    def length: Int = array.length
  }
}