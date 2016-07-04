package slark.optimizer

/**
 * @author a554114
 */
object Table {
  case class Cell(pre: String, post: String) {
    def fill(len: Int) = new String((View.OfString(pre).fill(len - post.length(), ' ') :++ View.OfString(post)).toArray)
    def length = pre.length() + post.length()
  }
  
  def mkString(table: View.Indexed[View.Indexed[Cell]], colSep: String, rowSep: String): String = {
    val array = table.map(_.toArray).toArray
    val len = View.OfRange(0, array(0).length).map(col => View.OfArray(array).map(_(col).length).max).toArray
    View.OfArray(array).map(row => View.OfRange(0, row.length).map(col => row(col).fill(len(col))).mkString(colSep)).mkString(rowSep)
  }
}