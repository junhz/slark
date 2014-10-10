package slark.xml

trait Element
object Element {
  def attrsString(attrs: Map[String, String]) = {
    def rec(sb: Vector[String], attrs: Map[String, String]): String = {
      if (attrs.isEmpty) sb.mkString
      else {
        val (name, value) = attrs.head
        val q = if (value.contains("'")) '"' else '"'
        rec(sb :+ s" $name=$q$value$q", attrs.tail)
      }
    }
    
    rec(Vector(), attrs)
  }
}

case class EmptyElement(val name: String, val attrs: Map[String, String]) extends Element {
  override def toString = s"<$name${Element.attrsString(attrs)}/>"
}
case class TextElement(val name: String, val attrs: Map[String, String], val content: String) extends Element {
  override def toString = s"<$name${Element.attrsString(attrs)}>$content</$name>"
}
case class NodeElement(val name: String, val attrs: Map[String, String], val content: List[Element]) extends Element {
  override def toString = {
    @annotation.tailrec
    def rec(sb: Vector[String], nodes: List[String], tab: String, elementss: List[List[Element]]): String = {
      val elements = elementss.head
      if (elements.isEmpty) {
        if (nodes.isEmpty) sb.mkString
        else rec(sb :+ tab.substring(2) :+ s"</${nodes.head}>\r\n", nodes.tail, tab.substring(2), elementss.tail)
      } else {
        val element = elements.head
        element match {
          case NodeElement(name, attrs, content) => rec(sb :+ tab :+ s"<$name${Element.attrsString(attrs)}>\r\n", name :: nodes, tab + "  ", content :: elementss.head.tail :: elementss.tail)
          case _ => rec(sb :+ tab :+ s"$element\r\n", nodes, tab, elementss.head.tail :: elementss.tail)
        }
      }
    }
    
    rec(Vector(), Nil, "", (this :: Nil) :: Nil)
  }
}