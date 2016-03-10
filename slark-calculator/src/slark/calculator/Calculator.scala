package slark.calculator

/**
 * @author a554114
 */
object Calculator {
  val combinatorCalculator = new CombinatorParses().test
  
  def main(args: Array[String]): Unit = {
    println(combinatorCalculator.parse(Input("1", 0)))
  }
  
}