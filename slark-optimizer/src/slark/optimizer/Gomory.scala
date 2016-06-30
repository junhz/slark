package slark.optimizer

/**
 * @author a554114
 */
object Gomory extends CuttingPlane {
  
  def apply(problem: Simplex.StandardForm) = {
    def f(r: Rational) = r.floor - r
    
    problem.newSlacks(View.Range(0, problem.n) some (!problem.b(_).isInteger) map {
      row => (problem.a(row).map(f), f(problem.b(row)))
    })
  }
  
}