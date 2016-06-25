package slark.optimizer

import java.util.NoSuchElementException

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
    final def forall(f: A => Boolean): Boolean = {
      val it = iterator
      var r = true
      while (it.hasNext) {
        r &= f(it.next())
      }
      r
    }
    final def some(cond: A => Boolean): Travesal[A] = TravesalTravesal(self, cond)
    
    final def toList: java.util.List[A] = {
      val l = new java.util.LinkedList[A]()
      val it = iterator
      while (it.hasNext) {
        l.add(it.next())
      }
      l
    }
  }
  
  case class List[A](list: java.util.List[A]) extends Travesal[A] {
    def iterator = {
      val it = list.iterator()
      new Iterator[A] {
        def hasNext = it.hasNext()
        def next = it.next()
      }
    }
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
    final def count(f: A => Boolean): Int = {
      val len = length
      var count = 0
      var idx = 0
      while (idx < len) {
        if (f(apply(idx))) count += 1
        else ()
        idx += 1
      }
      count
    }
    final def first(f: A => Boolean): A = {
      val len = length
      var found: Option[A] = None
      var idx = 0
      while (found.isEmpty && idx < len) {
        val a = apply(idx)
        if (f(a)) found = Some(a)
        else ()
        idx += 1
      }
      found match {
        case Some(a) => a
        case _ => throw new NoSuchElementException("no element matched")
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
        val len = self.length
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
    final def update(pos: Int, value: A): Indexed[A] = IndexedUpdated(self, pos, value)
    
    final def fill(length: Int, value: A): Indexed[A] = IndexedFilled(self, length, value)
    
    final def fold[B](b: B, f: (A, B) => B): B = {
      var r = b
      val len = length
      var idx = 0
      while (idx < len) {
        r = f(apply(idx), r)
        idx += 1
      }
      r
    }
    
    final def :++(that: Indexed[A]): Indexed[A] = IndexedAppendedIndexed(self, that)
    
    final def :+(a: A): Indexed[A] = IndexedAppended(self, a)
    
    final def +:(a: A): Indexed[A] = IndexedPrepended(self, a)
    
    final def tail() = IndexedRanged(self, 1, length)
    
    final override def toString = s"[${mkString(", ")}]"
  }
  
  case class Array[A](array: scala.Array[A]) extends Indexed[A] {
    def length = array.length
    def apply(idx: Int) = array(idx)
  }
  
  case class Range(start: Int, end: Int/*exclude*/) extends Indexed[Int] {
    def length = end - start
    def apply(idx: Int) = idx + start
  }
  
  case class Vector[A](vector: scala.collection.immutable.Vector[A]) extends Indexed[A] {
    def length = vector.length
    def apply(idx: Int) = vector(idx)
  }
  
  case class IndexedMapped[A, B](underlying: Indexed[A], mapping: A => B) extends Indexed[B] {
    def length = underlying.length
    def apply(idx: Int) = mapping(underlying(idx))
  }
  
  case class IndexedRanged[A](underlying: Indexed[A], from: Int, to: Int/*excluded*/) extends Indexed[A] {
    def length = to - from
    def apply(idx: Int) = underlying(idx + from)
  }
  
  case class IndexedUpdated[A](underlying: Indexed[A], pos: Int, value: A) extends Indexed[A] {
    def length = underlying.length
    def apply(idx: Int) = if (idx == pos) value else underlying(idx)
  }
  
  case class IndexedFilled[A](underlying: Indexed[A], length: Int, value: A) extends Indexed[A] {
    val len = underlying.length
    def apply(idx: Int) = if (idx < len) underlying(idx) else value
  }
  
  case class IndexedAppended[A](underlying: Indexed[A], value: A) extends Indexed[A] {
    val len = underlying.length
    def length = len + 1
    def apply(idx: Int) = if (idx < len) underlying(idx) else value
  }
  
  case class IndexedAppendedIndexed[A](underlying: Indexed[A], appended: Indexed[A]) extends Indexed[A] {
    val len = underlying.length
    def length = len + appended.length
    def apply(idx: Int) = {
      if (idx < len) underlying(idx) else appended(idx - len)
    }
  }
  
  case class IndexedPrepended[A](underlying: Indexed[A], value: A) extends Indexed[A] {
    def length = underlying.length + 1
    def apply(idx: Int) = if (idx == 0) value else underlying(idx - 1)
  }
  
  case class Cols[A](array: scala.Array[scala.Array[A]], length: Int) extends Indexed[Indexed[A]] {
    def apply(col: Int) = new Indexed[A] {
      def apply(row: Int) = array(row)(col)
      def length: Int = array.length
    }
  }
  case class Rows[A](array: scala.Array[scala.Array[A]]) extends Indexed[Indexed[A]] {
    val colLen = Array(array).map(_.length).max
    def apply(row: Int) = new Indexed[A] {
      def apply(col: Int) = array(row)(col)
      def length: Int = colLen
    }
    def length: Int = array.length
  }
  
  def empty[A](): Indexed[A] = new Indexed[A] {
    def length = 0
    def apply(idx: Int) = throw new NoSuchElementException(s"$idx of empty")
  }
}