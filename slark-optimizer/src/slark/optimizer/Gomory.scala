package slark.optimizer

/**
 * @author a554114
 */
object Gomory extends CuttingPlane {
  
  def apply(problem: Simplex.StandardForm) = {
    def f(r: Rational) = r.floor - r
    val cuttable = View.Array(problem.c).indexed.some(_._2.isZero) map {
      case (col, _) => {
        val row = View.Cols(problem.a)(col).indexed.maxBy(_._2)._1
        (row, col)
      }
    } some {
      case (row, col) => !problem.b(row).isInteger
    }
    val it = cuttable.iterator
    var p = problem
    while (it.hasNext) {
      val (row, col) = it.next()
      p = p.withConstraint(i => f(p.a(row)(i)), f(p.b(row)))
    }
    p
  }
  
}