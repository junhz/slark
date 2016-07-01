package slark.optimizer

/**
 * @author a554114
 */
case class Tapped(pre: String, post: String) {
  def fill(len: Int) = new String((View.OfString(pre).fill(len - post.length(), ' ') :++ View.OfString(post)).toArray)
  def length = pre.length() + post.length()
}