package slark.notepad

import scala.annotation.tailrec

/**
 * @author a554114
 */
trait Document {

  trait Item {
    def lines: Traversable[List[Char]]
  }

  class Title(val s: String) extends Item {
    val c = '#'
    val span = 6

    override def lines = {
      val len = s.length()
      val h = List.fill(len + span * 2 + 2)(c)
      val left = (c :: List.fill(span)(' '))
      val right = (List.fill(span)(' ') ::: List(c))
      h :: (left ::: s.toList ::: right) :: h :: Nil
    }
  }

  case class Snippet(val kind: String, val content: String) extends Item {
    val hc = '|'
    val vc = '-'
    val bs = 1
    val as = 6

    override def lines = {
      val rows = content.split(hc).map(_.replaceAll("""(?m)\s+$""", ""))
      val nLen = kind.length()
      val cLen = rows.map(_.length()).max
      val tLen = cLen + bs + as + 2
      val top = kind.toList ::: List.fill(tLen - nLen)(vc)
      val bottom = List.fill(tLen)(vc)
      val left = hc :: List.fill(bs)(' ')
      val right = List.fill(as)(' ') ::: List(hc)
      val c = rows.map(s => left ::: s.toList ::: List.fill(cLen - s.length())(' ') ::: right)
      top :: c.toList ::: List(bottom)
    }
  }
  
  case class Section(val desc: String, val items: List[Item], val sections: List[Section]) {
    def detail(items: Item*) = Section(desc, items.toList, sections)
    def children(sections: Section*) = Section(desc, items, sections.toList)
    def <-|(sections: Section*) = Section(desc, items, sections.toList)
    def ->|(items: Item*) = Section(desc, items.toList, sections)
  }
  
  implicit val section: String => Section = (desc: String) => Section(desc, Nil, Nil)
  
  def title: String
  
  def content: List[Section]
  
  final def main(args: Array[String]): Unit = {
    for (s <- new Title(title).lines) {
      println(s.mkString)
    }
    printSections(List(""), List(""), List(0), List(content))
  }
  
  def len(i: Int): Int = {
    @tailrec
    def rec(i: Int, len: Int): Int = {
      if (i < 10) len + 1
      else rec(i / 10, len + 1)
    }
    rec(i, 0)
  }
  
  @tailrec
  private def printSections(idents: List[String], secs: List[String], counts: List[Int], sectionss: List[List[Section]]): Unit = {
    if (sectionss.isEmpty) ()
    else {
      val sections = sectionss.head
      if (sections.isEmpty) {
        printSections(idents.tail, secs.tail, counts.tail, sectionss.tail)
      } else {
        val section = sections.head
        val count = counts.head + 1
        val ident = idents.head
        val sec = secs.head
        val newSec = s"$sec$count."
        val newIdent = List.fill(newSec.length() + 1)(' ').mkString + ident
        println(s"$ident$newSec ${section.desc}")
        for (item <- section.items) {
          for (s <- item.lines) {
            print(newIdent)
            println(s.mkString)
          }
        }
        printSections(newIdent :: idents, newSec :: secs, 0 :: count :: counts.tail, section.sections :: sections.tail :: sectionss.tail)
      }
    }
  }
  
}