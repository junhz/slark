package slark.xml

trait CharStream {

  def atEnd: Boolean
  def read: (Char, CharStream)
}

object CharStream {
  
  def fromString(s: String): CharStream = {
    def cs(index: Int): CharStream = new CharStream {
      def atEnd = index >= s.length()
      def read = (s.charAt(index), cs(index + 1))
    }
    
    cs(0)
  }
  
}