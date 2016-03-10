package slark.calculator

/**
 * @author a554114
 */
object Calculator {
  val combinatorCalculator = new CombinatorParses().calculator
  
  def main(args: Array[String]): Unit = {
    println(combinatorCalculator.parse(Input("(1 + 2 * 3 + 9) * 2 + 1", 0)))
  }
  
}