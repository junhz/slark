package test

object CacheDiary {

  def f = { 
    val i = CacheDiary
  slark.Cache(i) {
      () => i
    }
  }
  
}