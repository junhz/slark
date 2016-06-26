package slark.optimizer

/**
 * @author a554114
 */
object Gomory extends CuttingPlane {
  
  def apply(problem: Simplex.StandardForm) = {
    def f(r: Rational) = r.floor - r
    
    problem.strict(View.Range(0, problem.n.length) some (!problem.b(_).isInteger) map {
      row => (problem.a(row).map(f), f(problem.b(row)))
    })
  }
  
}