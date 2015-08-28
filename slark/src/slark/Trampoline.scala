package slark

object Trampolines {
  
  abstract class IsTrampoline[T] {
    def bounce(t: T): T
    def isDone(t: T): Boolean
    @tailrec
    final def run(t: T): T = {
      if (isDone(t)) t
      else run(bounce(t))
    }
  }
  
}