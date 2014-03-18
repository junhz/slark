package test
package bug

object `NPE when init object` {
  trait Has[T] {
    def create: T
    final val get: T = create
  }
  
  abstract class Clazz extends Has[Has[Int]]
  
  object Obj extends Clazz { self =>
    val i = 1
    override def create = new Has[Int] {
      override def create = i
    }
  }

  def main(args: Array[String]) {
    println(Obj.get)
  }
}


  