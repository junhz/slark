package slark

object FuncLib {

  abstract class Funct1[A, F] { self =>
    def `then`[U](fn: A => U): Funct1[U, F] = new Funct1[U, F] {
      override def apply(f: F): U = fn(self.apply(f))
    }
    final def and[B, F_](that: Funct1[B, F_]): Funct2[A, B, (F, F_)] = new Funct2[A, B, (F, F_)] {
      def apply(ff: (F, F_)): (A, B) = ff match { case (f, f_) => (self.apply(f), that.apply(f_)) }
    }
    final def default(a: A): Funct1[A, Option[F]] = new Funct1[A, Option[F]] {
      override def apply(f: Option[F]): A = f match {
        case None => a
        case Some(s) => self.apply(s)
      }
    }
    def apply(f: F): A
  }

  abstract class Funct2[A, B, F] { self =>
    final def `then`[U](fn: (A, B) => U): Funct1[U, F] = new Funct1[U, F] {
      override def apply(f: F): U = self.apply(f) match { case (a, b) => fn(a, b) }
    }
    final def `with`[T](t: T): Funct3[A, B, T, F] = new Funct3[A, B, T, F] {
      def `then`[U](fn: A => B => T => U): F => U = (f: F) => self.apply(f) match {
        case (a, b) => fn(a)(b)(t)
      }
    }
    final def and[C, F_](that: Funct1[C, F_]): Funct3[A, B, C, (F, F_)] = new Funct3[A, B, C, (F, F_)] {
      def `then`[U](fn: A => B => C => U): ((F, F_)) => U = (ff: (F, F_)) => ff match {
        case (f, f_) => self.apply(f) match { case (a, b) => fn(a)(b)(that.apply(f_)) }
      }
    }
    final def default(a: A, b: B): Funct2[A, B, Option[F]] = new Funct2[A, B, Option[F]] {
      override def apply(f: Option[F]): (A, B) = f match {
        case None => (a, b)
        case Some(s) => self.apply(s)
      }
    }
    def apply(f: F): (A, B)
  }

  abstract class Funct3[A, B, C, F] {
    def `then`[U](f: A => B => C => U): F => U
    final def `then`[U](f: (A, B, C) => U): F => U = `then`(f.curried)
  }

  def as[T]: Funct1[T, T] = new Funct1[T, T] {
    override def apply(t: T): T = t
  }

  def open2[A, B]: Funct2[A, B, (A, B)] = new Funct2[A, B, (A, B)] {
    override def apply(f: (A, B)): (A, B) = f
  }

}