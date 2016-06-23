package slark.optimizer

/**
 * @author a554114
 */
object Gomory extends CuttingPlane {
  
  def apply(problem: Simplex.StandardForm) = {
    def f(r: Rational) = r.floor - r
    val basicVars = problem.basicVars()
    
    problem.strict(basicVars.some(!_._2.isInteger) map {
      case (col, bi, ai) => (ai.map(f), f(bi))
    })
  }
  
}