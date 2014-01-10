package slark

object FuncLib {
  
  abstract class Funct1[A, F] {
    def apply[U](f: A => U): F => U
  }
  
  abstract class Funct2[A, B, F] { self =>
    def apply[U](f: A => B => U): F => U
    final def apply[U](f: (A, B) => U): F => U = apply(f.curried)
    final def `with`[T](t: T): Funct3[A, B, T, F] = new Funct3[A, B, T, F] {
      def apply[U](f: A => B => T => U): F => U = (ff: F) => self.apply(f)(ff)(t)
    }
    final def append[C, F_](that: Funct1[C, F_]): Funct3[A, B, C, (F, F_)] = new Funct3[A, B, C, (F, F_)] {
      def apply[U](f: A => B => C => U): ((F, F_)) => U = (ff: (F, F_)) => that.apply(self.apply(f)(ff._1))(ff._2)
    }
  }
  
  abstract class Funct3[A, B, C, F] {
    def apply[U](f: A => B => C => U): F => U
    final def apply[U](f: (A, B, C) => U): F => U = apply(f.curried)
  }
  
  def as[T]: Funct1[T, T] = new Funct1[T, T] {
    def apply[U](f: T => U): T => U = (t: T) => f(t)
  }
  
  def open2[A, B]: Funct2[A, B, (A, B)] = new Funct2[A, B, (A, B)] {
    override def apply[U](f: A => B => U): ((A, B)) => U = (tuple: (A, B)) => f(tuple._1)(tuple._2)
  }
  
}