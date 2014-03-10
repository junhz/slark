package test

object CacheDiary {

  def f(a: A): () => a.B = {
    slark.Cache(a) {
      () => a.f
    }
  }

}

class A {
  class B {
  }

  def f: B = new B
}