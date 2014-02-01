package slark

object Streams {

  def fromList[T](ls: List[T], default: T): Stream[T] = {
    if(ls.isEmpty) default #:: fromList(Nil, default)
    else ls.head #:: fromList(ls.tail, default)
  }
  
}