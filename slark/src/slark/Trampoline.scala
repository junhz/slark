package slark

object Trampolines {
  trait Trampoline[+T] {
    @tailrec
    final def run: T = {
      this match {
        case Done(r) => r
        case Next1(fun, in) => fun(in).run
        case FlatMap(pre, fun) => (pre match {
          case Done(r) => {
            fun(r)
          }
          case Next1(f, i) => FlatMap(f(i), fun)
          case FlatMap(p, f) => FlatMap(p, associate(f, fun))
        }).run
      }
    }
  }
  case class Done[T](result: T) extends Trampoline[T]
  case class Next1[I, T](fun: I => Trampoline[T], in: I) extends Trampoline[T]
  case class FlatMap[T, P](pre: Trampoline[P], fun: P => Trampoline[T]) extends Trampoline[T]
  
  def associate[T1, T2, T3](f: T1 => Trampoline[T2], g: T2 => Trampoline[T3]): T1 => Trampoline[T3] = (result: T1) => FlatMap(f(result), g)
  
}