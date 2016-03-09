package slark.calculator

case class Input(val txt: String, val pos: Int) {
  def atEnd = !(pos < txt.length())
  def next = Input(txt, pos + 1)
  def cnt = txt.charAt(pos)
}