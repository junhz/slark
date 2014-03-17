package test

package object bug {

  trait A
  
  trait HasA[+T <: A] {
    protected[this] def _a: T
    final val a = _a
  }
  
  trait UseA[+T <: A] {
    protected[this] def _a: T
    final val a = _a
  }
  
  trait Use[T <: HasA[A]] {
    protected[this] def _hasA: T
    final val hasA = _hasA
    
    protected[this] def _useA: UseA[hasA.a.type]
    final val useA = _useA
  }
  
  object Obj extends Use[HasA[A]] { self =>
    
    override def _hasA = new HasA[A] {
      override def _a = new A {}
    }
    override def _useA = new UseA[hasA.a.type] {
      override def _a = self.hasA.a
    }
    
  }
  
}