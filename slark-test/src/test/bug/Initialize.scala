package test.bug

object Initialize {

  trait A {
    val i: String
    val j = i + " omg"
  }
  
  final class B extends A {
    override val i = "ohhh"
  }
  
  def main(args: Array[String]): Unit = {
    println(new B().j)
    println(new D().j)
  }
  
  trait C {
    val i: String
    val j = i + " omg"
  }
  
  final class D extends { val i = "ohhh" } with C
  
}