package slark.xml

import java.nio.charset.Charset
import com.sun.corba.se.impl.ior.ByteBuffer
import java.nio.CharBuffer

trait Parser[T] { self =>
  def parse(cs: CharStream): (T, CharStream)
}

object Parsers {

  def toNextChar(c: Char) = new Parser[String] {
    override def parse(cs: CharStream) = {
      val sb = new StringBuilder
      var (cnt, next) = cs.read
      while (cnt != c) {
        sb.append(cnt)
        val tmp = next.read
        cnt = tmp._1
        next = tmp._2
      }
      (sb.toString, next)
    }
  }

  def toNextCharIn(chars: Char*) = new Parser[(String, Char)] {
    override def parse(cs: CharStream) = {
      val sb = new StringBuilder
      var (cnt, next) = cs.read
      while (!chars.contains(cnt)) {
        sb.append(cnt)
        val tmp = next.read
        cnt = tmp._1
        next = tmp._2
      }
      ((sb.toString, cnt), next)
    }
  }

  val attrValue = new Parser[String] {
    override def parse(cs: CharStream) = {
      val ((_, c), next) = toNextCharIn(''', '"').parse(cs)
      toNextChar(c).parse(next)
    }
  }

  val tag = new Parser[Tag] {
    override def parse(cs: CharStream) = {

      @annotation.tailrec
      def readAttrs(attrs: Map[String, String], cs: CharStream): (Map[String, String], Char, CharStream) = {
        val tmp = toNextCharIn('=', '>', '/').parse(cs)
        tmp._1._2 match {
          case '=' => {
            val (v, n) = attrValue.parse(tmp._2)
            readAttrs(attrs + ((tmp._1._1.trim(), v)), n)
          }
          case '/' => {
            (attrs, '/', tmp._2)
          }
          case '>' => {
            (attrs, '>', tmp._2)
          }
        }
      }

      val ((s, c), next1) = toNextCharIn(' ', '/', '>').parse(cs)
      c match {
        case ' ' => {
          if (s.length() == 0) parse(next1)
          else {
            val tmp = readAttrs(Map(), next1)
            tmp._2 match {
              case '/' => {
                (EmptyElemTag(s, tmp._1), toNextChar('>').parse(tmp._3)._2)
              }
              case '>' => {
                (STag(s, tmp._1), tmp._3)
              }
            }
          }
        }
        case '/' if s.length() == 0 => {
          val tmp = toNextChar('>').parse(next1)
          (ETag(tmp._1.trim()), tmp._2)
        }
        case '/' if s.length() > 0 => {
          (EmptyElemTag(s, Map()), toNextChar('>').parse(next1)._2)
        }
        case '>' => (STag(s, Map()), next1)
      }
    }
  }
  
  def consistOf(s: String, chars: Char*): Boolean = {
    @annotation.tailrec
    def rec(s: String, index: Int): Boolean = {
      if (index >= s.length()) true
      else {
        if (chars.contains(s.charAt(index))) rec(s, index + 1)
        else false
      }
    }
    rec(s, 0)
  }

  val element = new Parser[Element] {
    override def parse(cs: CharStream) = {

      def push[T](lss: List[List[T]], t: T): List[List[T]] = {
        if (lss.isEmpty) (t :: Nil) :: Nil
        else (t :: lss.head) :: lss.tail
      }
      
      @annotation.tailrec
      def rec(contentss: List[List[Element]], sTags: List[STag], cs: CharStream): (Element, CharStream) = {
        if (sTags.isEmpty && !contentss.isEmpty) (contentss.head.head, cs)
        else {
          val (text, n1) = toNextChar('<').parse(cs)
          val (t, n2) = tag.parse(n1)
          if (text.length() > 0 && !consistOf(text, '\r', '\n', '\t', ' ')) {
            t match {
              case ETag(name) if (contentss.head.isEmpty && sTags.head.name.equals(name)) => rec(push(contentss.tail, TextElement(name, sTags.head.attrs, text)), sTags.tail, n2)
            }
          } else {
            t match {
              case EmptyElemTag(name, attrs) => rec(push(contentss, EmptyElement(name, attrs)), sTags, n2)
              case s: STag => rec(Nil :: contentss, s :: sTags, n2)
              case ETag(name) if (sTags.head.name.equals(name)) => {
                if (contentss.head.isEmpty && (text.length() == 0 || consistOf(text, '\t', ' '))) {
                  rec(push(contentss.tail, TextElement(name, sTags.head.attrs, text)), sTags.tail, n2)
                } else {
                  rec(push(contentss.tail, NodeElement(name, sTags.head.attrs, contentss.head.reverse)), sTags.tail, n2)
                }
              }
            }
          }
        }
      }
      
      rec(Nil, Nil, cs)
    }
  }
}