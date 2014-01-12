package slark

object FuncLib {
  
  abstract class Funct1[A, F] { self =>
    def `then`[U](f: A => U): F => U
    final def and[B, F_](that: Funct1[B, F_]): Funct2[A, B, (F, F_)] = new Funct2[A, B, (F, F_)] {
      def `then`[U](f: A => B => U): ((F, F_)) => U = (ff: (F, F_)) => that.`then`(self.`then`(f)(ff._1))(ff._2)
    }
    final def default(a: A): Funct1[A, Option[F]] = new Funct1[A, Option[F]] {
      override def `then`[U](f: A => U): Option[F] => U = _ match {
        case None => f(a)
        case Some(s) => self.`then`(f)(s)
      }
    }
  }
  
  abstract class Funct2[A, B, F] { self =>
    def `then`[U](f: A => B => U): F => U
    final def `then`[U](f: (A, B) => U): F => U = `then`(f.curried)
    final def `with`[T](t: T): Funct3[A, B, T, F] = new Funct3[A, B, T, F] {
      def `then`[U](f: A => B => T => U): F => U = (ff: F) => self.`then`(f)(ff)(t)
    }
    final def and[C, F_](that: Funct1[C, F_]): Funct3[A, B, C, (F, F_)] = new Funct3[A, B, C, (F, F_)] {
      def `then`[U](f: A => B => C => U): ((F, F_)) => U = (ff: (F, F_)) => that.`then`(self.`then`(f)(ff._1))(ff._2)
    }
    final def default(a: A, b: B): Funct2[A,B, Option[F]] = new Funct2[A, B, Option[F]] {
      override def `then`[U](f: A => B => U): Option[F] => U = _ match {
        case None => f(a)(b)
        case Some(s) => self.`then`(f)(s)
      }
    }
  }
  
  abstract class Funct3[A, B, C, F] {
    def `then`[U](f: A => B => C => U): F => U
    final def `then`[U](f: (A, B, C) => U): F => U = `then`(f.curried)
  }
  
  def as[T]: Funct1[T, T] = new Funct1[T, T] {
    def `then`[U](f: T => U): T => U = (t: T) => f(t)
  }
  
  def open2[A, B]: Funct2[A, B, (A, B)] = new Funct2[A, B, (A, B)] {
    override def `then`[U](f: A => B => U): ((A, B)) => U = (tuple: (A, B)) => f(tuple._1)(tuple._2)
  }
  
}