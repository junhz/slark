package slark.xml

trait Tag {

}

case class EmptyElemTag(name: String, attrs: Map[String, String]) extends Tag
case class STag(name: String, attrs: Map[String, String]) extends Tag
case class ETag(name: String) extends Tag