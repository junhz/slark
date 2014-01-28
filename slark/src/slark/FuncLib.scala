package slark

import scala.collection.mutable.ListBuffer

object FuncLib {

  abstract class Funct1[R, T] extends (T => R) { self =>
    final def `then`[R_](fn: R => R_): Funct1[R_, T] = new Funct1[R_, T] {
      override def apply(t: T): R_ = fn(self.apply(t))
    }
    final def and[R_, T_](that: Funct1[R_, T_]): Funct2[R, R_, (T, T_)] = new Funct2[R, R_, (T, T_)] {
      def apply(tt: (T, T_)): (R, R_) = tt match { case (t, t_) => (self.apply(t), that.apply(t_)) }
    }
    final def and[R1_, R2_, T_](that: Funct2[R1_, R2_, T_]): Funct3[R, R1_, R2_, (T, T_)] =
      new Funct3[R, R1_, R2_, (T, T_)] {
        def apply(tt: (T, T_)): (R, R1_, R2_) = that.apply(tt._2) match {
          case (r1_, r2_) => (self.apply(tt._1), r1_, r2_)
        }
      }
    final def and[R1_, R2_, R3_, T_](that: Funct3[R1_, R2_, R3_, T_]): Funct4[R, R1_, R2_, R3_, (T, T_)] =
      new Funct4[R, R1_, R2_, R3_, (T, T_)] {
        override def apply(tt: (T, T_)): (R, R1_, R2_, R3_) = that.apply(tt._2) match {
          case (r1_, r2_, r3_) => (self.apply(tt._1), r1_, r2_, r3_)
        }
      }
    final def and[R1_, R2_, R3_, R4_, T_](that: Funct4[R1_, R2_, R3_, R4_, T_]): Funct5[R, R1_, R2_, R3_, R4_, (T, T_)] =
      new Funct5[R, R1_, R2_, R3_, R4_, (T, T_)] {
        override def apply(tt: (T, T_)): (R, R1_, R2_, R3_, R4_) = that.apply(tt._2) match {
          case (r1_, r2_, r3_, r4_) => (self.apply(tt._1), r1_, r2_, r3_, r4_)
        }
      }
    final def and[R1_, R2_, R3_, R4_, R5_, T_](that: Funct5[R1_, R2_, R3_, R4_, R5_, T_]): Funct6[R, R1_, R2_, R3_, R4_, R5_, (T, T_)] =
      new Funct6[R, R1_, R2_, R3_, R4_, R5_, (T, T_)] {
        override def apply(tt: (T, T_)): (R, R1_, R2_, R3_, R4_, R5_) = that.apply(tt._2) match {
          case (r1_, r2_, r3_, r4_, r5_) => (self.apply(tt._1), r1_, r2_, r3_, r4_, r5_)
        }
      }
    final def and[R1_, R2_, R3_, R4_, R5_, R6_, T_](that: Funct6[R1_, R2_, R3_, R4_, R5_, R6_, T_]): Funct7[R, R1_, R2_, R3_, R4_, R5_, R6_, (T, T_)] =
      new Funct7[R, R1_, R2_, R3_, R4_, R5_, R6_, (T, T_)] {
        override def apply(tt: (T, T_)): (R, R1_, R2_, R3_, R4_, R5_, R6_) = that.apply(tt._2) match {
          case (r1_, r2_, r3_, r4_, r5_, r6_) => (self.apply(tt._1), r1_, r2_, r3_, r4_, r5_, r6_)
        }
      }
    final def default(r: R): Funct1[R, Option[T]] = new Funct1[R, Option[T]] {
      override def apply(t: Option[T]): R = t match {
        case None => r
        case Some(s) => self.apply(s)
      }
    }
    final def `with`[R_](r: R_): Funct2[R, R_, T] = new Funct2[R, R_, T] {
      override def apply(t: T): (R, R_) = (self.apply(t), r)
    }
    final def varargs: VarargsFunct[R, T] = new VarargsFunct(this `then` (Tuple1(_)))
    def apply(t: T): R
  }

  abstract class Funct2[R1, R2, T] { self =>
    final def `then`[R](fn: (R1, R2) => R): Funct1[R, T] = new Funct1[R, T] {
      override def apply(t: T): R = self.apply(t) match { case (r1, r2) => fn(r1, r2) }
    }
    final def `with`[R](r: R): Funct3[R1, R2, R, T] = new Funct3[R1, R2, R, T] {
      def apply(t: T): (R1, R2, R) = self.apply(t) match { case (r1, r2) => (r1, r2, r) }
    }
    final def and[R_, T_](that: Funct1[R_, T_]): Funct3[R1, R2, R_, (T, T_)] = new Funct3[R1, R2, R_, (T, T_)] {
      def apply(tt: (T, T_)): (R1, R2, R_) = tt match {
        case (t, t_) => self.apply(t) match { case (r1, r2) => (r1, r2, that.apply(t_)) }
      }
    }
    final def and[R1_, R2_, R3_, R4_, T_](that: Funct4[R1_, R2_, R3_, R4_, T_]): Funct6[R1, R2, R1_, R2_, R3_, R4_, (T, T_)] =
      new Funct6[R1, R2, R1_, R2_, R3_, R4_, (T, T_)] {
        override def apply(tt: (T, T_)): (R1, R2, R1_, R2_, R3_, R4_) = (self.apply(tt._1), that.apply(tt._2)) match {
          case ((a, b), (r1_, r2_, r3_, r4_)) => (a, b, r1_, r2_, r3_, r4_)
        }
      }
    final def default(r1: R1, r2: R2): Funct2[R1, R2, Option[T]] = new Funct2[R1, R2, Option[T]] {
      override def apply(t: Option[T]): (R1, R2) = t match {
        case None => (r1, r2)
        case Some(s) => self.apply(s)
      }
    }
    final def varargs[R_, R1_ >: R1 <: R_, R2_ >: R2 <: R_]: VarargsFunct[R_, T] = new VarargsFunct(this.apply)
    def apply(t: T): (R1, R2)
  }

  abstract class Funct3[R1, R2, R3, T] { self =>
    final def `then`[R](fn: (R1, R2, R3) => R): Funct1[R, T] = new Funct1[R, T] {
      override def apply(t: T): R = self.apply(t) match { case (r1, r2, r3) => fn(r1, r2, r3) }
    }
    final def and[R_, T_](that: Funct1[R_, T_]): Funct4[R1, R2, R3, R_, (T, T_)] = new Funct4[R1, R2, R3, R_, (T, T_)] {
      override def apply(tt: (T, T_)): (R1, R2, R3, R_) = self.apply(tt._1) match {
        case (r1, r2, r3) => (r1, r2, r3, that.apply(tt._2))
      }
    }
    final def and[R1_, R2_, R3_, T_](that: Funct3[R1_, R2_, R3_, T_]): Funct6[R1, R2, R3, R1_, R2_, R3_, (T, T_)] =
      new Funct6[R1, R2, R3, R1_, R2_, R3_, (T, T_)] {
        override def apply(tt: (T, T_)): (R1, R2, R3, R1_, R2_, R3_) = (self.apply(tt._1), (that.apply(tt._2))) match {
          case ((r1, r2, r3), (r1_, r2_, r3_)) => (r1, r2, r3, r1_, r2_, r3_)
        }
      }
    final def default(r1: R1, r2: R2, r3: R3): Funct3[R1, R2, R3, Option[T]] = new Funct3[R1, R2, R3, Option[T]] {
      override def apply(t: Option[T]): (R1, R2, R3) = t match {
        case None => (r1, r2, r3)
        case Some(s) => self.apply(s)
      }
    }
    final def `with`[R](r: R): Funct4[R1, R2, R3, R, T] = new Funct4[R1, R2, R3, R, T] {
      override def apply(t: T): (R1, R2, R3, R) = self.apply(t) match {
        case (r1, r2, r3) => (r1, r2, r3, r)
      }
    }
    final def varargs[R_, R1_ >: R1 <: R_, R2_ >: R2 <: R_, R3_ >: R3 <: R_]: VarargsFunct[R_, T] = new VarargsFunct(this.apply)
    def apply(t: T): (R1, R2, R3)
  }

  abstract class Funct4[R1, R2, R3, R4, T] { self =>
    final def and[R1_, R2_, T_](that: Funct2[R1_, R2_, T_]): Funct6[R1, R2, R3, R4, R1_, R2_, (T, T_)] =
      new Funct6[R1, R2, R3, R4, R1_, R2_, (T, T_)] {
        override def apply(tt: (T, T_)) = (self.apply(tt._1), that.apply(tt._2)) match {
          case ((r1, r2, r3, r4), (r1_, r2_)) => (r1, r2, r3, r4, r1_, r2_)
        }
      }
    final def default(r1: R1, r2: R2, r3: R3, r4: R4): Funct4[R1, R2, R3, R4, Option[T]] = new Funct4[R1, R2, R3, R4, Option[T]] {
      override def apply(t: Option[T]): (R1, R2, R3, R4) = t match {
        case None => (r1, r2, r3, r4)
        case Some(s) => self.apply(s)
      }
    }
    final def `with`[R](r: R): Funct5[R1, R2, R3, R4, R, T] = new Funct5[R1, R2, R3, R4, R, T] {
      override def apply(t: T): (R1, R2, R3, R4, R) = self.apply(t) match {
        case (r1, r2, r3, r4) => (r1, r2, r3, r4, r)
      }
    }
    final def `then`[R](fn: (R1, R2, R3, R4) => R): Funct1[R, T] = new Funct1[R, T] {
      override def apply(t: T): R = self.apply(t) match {
        case (r1, r2, r3, r4) => fn(r1 ,r2, r3, r4)
      }
    }
    def apply(t: T): (R1, R2, R3, R4)
  }

  abstract class Funct5[R1, R2, R3, R4, R5, T] { self =>
    final def `with`[R](r: R): Funct6[R1, R2, R3, R4, R5, R, T] = new Funct6[R1, R2, R3, R4, R5, R, T] {
      override def apply(t: T): (R1, R2, R3, R4, R5, R) = self.apply(t) match {
        case (r1, r2, r3, r4, r5) => (r1, r2, r3, r4, r5, r)
      }
    }
    final def and[R_, T_](that: Funct1[R_, T_]): Funct6[R1, R2, R3, R4, R5, R_, (T, T_)] =
      new Funct6[R1, R2, R3, R4, R5, R_, (T, T_)] {
        override def apply(tt: (T, T_)): (R1, R2, R3, R4, R5, R_) = self.apply(tt._1) match {
          case (r1, r2, r3, r4, r5) => (r1, r2, r3, r4, r5, that.apply(tt._2))
        }
      }
    final def default(r1: R1, r2: R2, r3: R3, r4: R4, r5: R5): Funct5[R1, R2, R3, R4, R5, Option[T]] =
      new Funct5[R1, R2, R3, R4, R5, Option[T]] {
        override def apply(t: Option[T]): (R1, R2, R3, R4, R5) = t match {
          case None => (r1, r2, r3, r4, r5)
          case Some(s) => self.apply(s)
        }
      }
    def apply(t: T): (R1, R2, R3, R4, R5)
  }

  abstract class Funct6[R1, R2, R3, R4, R5, R6, T] { self =>
    final def `then`[R](fn: (R1, R2, R3, R4, R5, R6) => R): Funct1[R, T] = new Funct1[R, T] {
      override def apply(t: T): R = self.apply(t) match {
        case (r1, r2, r3, r4, r5, r6) => fn(r1, r2, r3, r4, r5, r6)
      }
    }
    final def and[R1_, R2_, T_](that: Funct2[R1_, R2_, T_]): Funct8[R1, R2, R3, R4, R5, R6, R1_, R2_, (T, T_)] =
      new Funct8[R1, R2, R3, R4, R5, R6, R1_, R2_, (T, T_)] {
        override def apply(tt: (T, T_)): (R1, R2, R3, R4, R5, R6, R1_, R2_) = (self.apply(tt._1), that.apply(tt._2)) match {
          case ((r1, r2, r3, r4, r5, r6), (r1_, r2_)) => (r1, r2, r3, r4, r5, r6, r1_, r2_)
        }
      }
    final def default(r1: R1, r2: R2, r3: R3, r4: R4, r5: R5, r6: R6): Funct6[R1, R2, R3, R4, R5, R6, Option[T]] =
      new Funct6[R1, R2, R3, R4, R5, R6, Option[T]] {
        override def apply(t: Option[T]): (R1, R2, R3, R4, R5, R6) = t match {
          case None => (r1, r2, r3, r4, r5, r6)
          case Some(s) => self.apply(s)
        }
      }
    final def `with`[R](r: R): Funct7[R1, R2, R3, R4, R5, R6, R, T] = new Funct7[R1, R2, R3, R4, R5, R6, R, T] {
      override def apply(t: T): (R1, R2, R3, R4, R5, R6, R) = self.apply(t) match {
        case (r1, r2, r3, r4, r5, r6) => (r1, r2, r3, r4, r5, r6, r)
      }
    }
    def apply(t: T): (R1, R2, R3, R4, R5, R6)
  }

  abstract class Funct7[R1, R2, R3, R4, R5, R6, R7, T] { self =>
    def and[R_, T_](that: Funct1[R_, T_]): Funct8[R1, R2, R3, R4, R5, R6, R7, R_, (T, T_)] =
      new Funct8[R1, R2, R3, R4, R5, R6, R7, R_, (T, T_)] {
        override def apply(tt: (T, T_)): (R1, R2, R3, R4, R5, R6, R7, R_) = self.apply(tt._1) match {
          case (r1, r2, r3, r4, r5, r6, r7) => (r1, r2, r3, r4, r5, r6, r7, that.apply(tt._2))
        }
      }
    final def default(r1: R1, r2: R2, r3: R3, r4: R4, r5: R5, r6: R6, r7: R7): Funct7[R1, R2, R3, R4, R5, R6, R7, Option[T]] =
      new Funct7[R1, R2, R3, R4, R5, R6, R7, Option[T]] {
        override def apply(t: Option[T]): (R1, R2, R3, R4, R5, R6, R7) = t match {
          case None => (r1, r2, r3, r4, r5, r6, r7)
          case Some(s) => self.apply(s)
        }
      }
    final def `with`[R](r: R): Funct8[R1, R2, R3, R4, R5, R6, R7, R, T] = new Funct8[R1, R2, R3, R4, R5, R6, R7, R, T] {
      override def apply(t: T): (R1, R2, R3, R4, R5, R6, R7, R) = self.apply(t) match {
        case (r1, r2, r3, r4, r5, r6, r7) => (r1, r2, r3, r4, r5, r6, r7, r)
      }
    }
    def apply(t: T): (R1, R2, R3, R4, R5, R6, R7)
  }

  abstract class Funct8[R1, R2, R3, R4, R5, R6, R7, R8, T] { self =>
    final def `then`[R](fn: (R1, R2, R3, R4, R5, R6, R7, R8) => R): Funct1[R, T] = new Funct1[R, T] {
      override def apply(t: T): R = self.apply(t) match {
        case (r1, r2, r3, r4, r5, r6, r7, r8) => fn(r1, r2, r3, r4, r5, r6, r7, r8)
      }
    }
    def apply(t: T): (R1, R2, R3, R4, R5, R6, R7, R8)
  }
  
  final class VarargsFunct[R, T](fn: T => Product) extends Funct1[List[R], T] {
    def apply(t: T): List[R] = {
      val it = fn(t).productIterator
      val buffer = new ListBuffer[R]
      while(it.hasNext) {
        buffer.append(it.next.asInstanceOf[R])
      }
      buffer.toList
    }
  }

  def as[T]: Funct1[T, T] = new Funct1[T, T] {
    override def apply(t: T): T = t
  }

  def open2[A, B]: Funct2[A, B, (A, B)] = new Funct2[A, B, (A, B)] {
    override def apply(f: (A, B)): (A, B) = f
  }

  def takeOption1[T](defaultValue: T): Funct1[T, List[T]] = new Funct1[T, List[T]] {
    override def apply(t: List[T]): T = t.headOption match {
      case None => defaultValue
      case Some(s) => s
    }
  }

  def takeOption2[T](defaultValue: T): Funct2[T, T, List[T]] = new Funct2[T, T, List[T]] {
    override def apply(t: List[T]): (T, T) = Streams.fromList(t, defaultValue) match {
      case t1 #:: t2 #:: _ => (t1, t2)
    }
  }

  def takeOption3[T](defaultValue: T): Funct3[T, T, T, List[T]] = new Funct3[T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T) = Streams.fromList(t, defaultValue) match {
      case t1 #:: t2 #:: t3 #:: _ => (t1, t2, t3)
    }
  }

  def takeOption4[T](defaultValue: T): Funct4[T, T, T, T, List[T]] = new Funct4[T, T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T, T) = Streams.fromList(t, defaultValue) match {
      case t1 #:: t2 #:: t3 #:: t4 #:: _ => (t1, t2, t3, t4)
    }
  }

  def takeOption5[T](defaultValue: T): Funct5[T, T, T, T, T, List[T]] = new Funct5[T, T, T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T, T, T) = Streams.fromList(t, defaultValue) match {
      case t1 #:: t2 #:: t3 #:: t4 #:: t5 #:: _ => (t1, t2, t3, t4, t5)
    }
  }

  def takeOption6[T](defaultValue: T): Funct6[T, T, T, T, T, T, List[T]] = new Funct6[T, T, T, T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T, T, T, T) = Streams.fromList(t, defaultValue) match {
      case t1 #:: t2 #:: t3 #:: t4 #:: t5 #:: t6 #:: _ => (t1, t2, t3, t4, t5, t6)
    }
  }

  def take2[T]: Funct2[T, T, List[T]] = new Funct2[T, T, List[T]] {
    override def apply(t: List[T]): (T, T) = t match {
      case t1 :: t2 :: _ => (t1, t2)
    }
  }

  def take3[T]: Funct3[T, T, T, List[T]] = new Funct3[T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T) = t match {
      case t1 :: t2 :: t3 :: _ => (t1, t2, t3)
    }
  }

  def take4[T]: Funct4[T, T, T, T, List[T]] = new Funct4[T, T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T, T) = t match {
      case t1 :: t2 :: t3 :: t4 :: _ => (t1, t2, t3, t4)
    }
  }

  def take5[T]: Funct5[T, T, T, T, T, List[T]] = new Funct5[T, T, T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T, T, T) = t match {
      case t1 :: t2 :: t3 :: t4 :: t5 :: _ => (t1, t2, t3, t4, t5)
    }
  }

  def take6[T]: Funct6[T, T, T, T, T, T, List[T]] = new Funct6[T, T, T, T, T, T, List[T]] {
    override def apply(t: List[T]): (T, T, T, T, T, T) = t match {
      case t1 :: t2 :: t3 :: t4 :: t5 :: t6 :: _ => (t1, t2, t3, t4, t5, t6)
    }
  }

}