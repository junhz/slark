package test.bug

object Structure {

  trait AS {
    trait A {
      def f: Unit
    }
  
    val a: () => A { def g: Unit }
  }
  
  val as = new AS {
    final class AA extends A {
      def f: Unit = println(1)
      def g: Unit = println(2)
    }
    val a: () => A { def g: Unit } = () => new AA
  }
  
  //as.a().g
  
}