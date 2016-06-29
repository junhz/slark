package slark.optimizer

import java.math.BigInteger

/**
 * @author a554114
 */
trait Simplex {
  import Simplex._
  def solve(problem: StandardForm): SolveResult
}

object Simplex {
  trait Variable {
    def ord: Int
  }
  case class DecideVar(val ord: Int) extends Variable {
    override def toString = s"x$ord"
  }
  case class SlackVar(val ord: Int) extends Variable {
    override def toString = s"s+$ord"
  }
  case class SurplusVar(val ord: Int) extends Variable {
    override def toString = s"s-$ord"
  }
  case class ArtifactVar(var ord: Int) extends Variable {
    override def toString = s"a$ord"
  }
  case class StandardForm(a: View.Indexed[View.Indexed[Rational]], 
                          b: View.Indexed[Rational], 
                          c: View.Indexed[Rational], 
                          z: Rational,
                          nbv: View.Indexed[Variable],
                          bv: View.Indexed[Variable]) {
    val basicCount = bv.length
    val nonBasicCount = nbv.length
    val varSize = basicCount + nonBasicCount
    def newVariable(ai: View.Indexed[Rational], bi: Rational) = {
      val nai = ai.fill(basicCount, Rational.zero)
      StandardForm(a :+ nai, b :+ bi, c, z, nbv :+ SlackVar(nonBasicCount), bv)
    }
    
    def newVariables(variables: View.Travesal[(View.Indexed[Rational], Rational)]) = {
      val vars = variables.toList.toArray(Array[(View.Indexed[Rational], Rational)]())
      val na = View.Array(vars).map(_._1.fill(basicCount, Rational.zero))
      StandardForm(a :++ na, b :++ View.Array(vars).map(_._2), c, z, nbv :++ View.Range(0, vars.length).map(n => SlackVar(n + nonBasicCount)), bv)
    }
    
    // TODO: use LinearProgram.Constraint
    def subjectTo(coe: View.Indexed[Rational], bi: Rational): StandardForm = {
      val ncoe = coe.fill(varSize, Rational.zero)
      val nai = View.Range(0, basicCount).map(i => bv(i) match {
        case DecideVar(ord) => ncoe(ord)
        case _ => Rational.zero
      }).toArray
      var nbi = bi
      View.Range(0, nonBasicCount).foreach(
        row => {
          nbv(row) match {
            case DecideVar(ord) => {
              val factor = ncoe(ord)
              var idx = 0
              while (idx < basicCount) {
                nai(idx) -= factor * a(row)(idx)
                idx += 1
              }
              nbi -= factor * b(row)
            }
            case _ => ()
          }
        }
      )
      StandardForm(a :+ View.Array(nai), b :+ nbi, c, z, nbv :+ SlackVar(nonBasicCount), bv)
    }
    // TODO: use lhs rhs instead
    override def toString = {
      val max = {
        val coeStr = View.Range(0, basicCount).map(i => {
          val ci = c(i)
          if (ci.isZero) "" else { if(ci.isPositive) s"+ $ci${bv(i)}" else s"- ${ci.negate}${bv(i)}" }
        }).mkString(" ")
        s"max $z $coeStr"
      }
      
      val subjectTo = if (nonBasicCount > 0) {
        val firstIdx = Array.fill(nonBasicCount)(-1)
        View.Range(0, nonBasicCount).foreach(row => View.Range(0, basicCount).foreach(col => {
          if(firstIdx(row) < 0 && a(row)(col).isZero) firstIdx(row) = col
          else ()
        }))
        val aStr = View.Range(0, nonBasicCount).map(row => View.Range(0, basicCount).map(col => {
          val aij = a(row)(col)
          if (aij.isZero) ""
          else {
            val n = bv(col)
            if (col == firstIdx(row)) {
              if (col == 0) s"$aij$n"
              else if (aij.isPositive) s"  $aij$n" else s"- ${aij.negate}$n"
            } else if (aij.isPositive) s"+ $aij$n" else s"- ${aij.negate}$n"
          }
        })).map(_.toArray).toArray
        val ajLen = View.Cols(aStr, basicCount).map(_.map(_.length).max).toArray
        View.Range(0, nonBasicCount).map(row => {
          val lhs = View.Range(0, basicCount).map(col => {
            val s = aStr(row)(col)
            new String(Array.fill(ajLen(col) - s.length())(' ')) + s
          }).mkString(" ")
          s"$lhs = ${b(row)} - ${nbv(row)}"
        })
      } else View.empty[String]()
      
      (max +: subjectTo).mkString("\r\n")
    }
  }

  sealed trait SolveResult

  case class Optimized(solution: StandardForm) extends SolveResult {
    override def toString = s"Optimized\r\n$solution"
  }
  case object Infeasible extends SolveResult
  case object Unbounded extends SolveResult
  
  trait Phase {

    def solve(problem: StandardForm): SolveResult

    final def pivot(matrix: Array[Array[Rational]], row: Int, col: Int): Unit = {
      //println(s"pivot at ($row, $col)")
      var r = 0
      val pr = View.Array(matrix(row)).toArray
      while (r < matrix.length) {
        val vector = matrix(r)
        var c = 0
        val factor = vector(col)
        while (c < vector.length) {
          if (r == row) {
            if (c == col) vector(c) = Rational.one / factor
            else vector(c) /= factor
          } else {
            if (c == col) vector(c) = -factor / pr(col)
            else vector(c) -= pr(c) * factor / pr(col)
          }
          c += 1
        }
        r += 1
      }
      //show(matrix)
    }

    final def show(matrix: Array[Array[Rational]]): Unit = {
      val textLen = View.Cols(matrix, matrix(0).length).map(arr => arr.map(_.toString().length()).max).toArray
      val s = View.Rows(matrix).map(arr => View.Range(0, textLen.length).map(col => {
        val s = arr(col).toString()
        new String(Array.fill(textLen(col) - s.length)(' ')) + s
      }).mkString(" "))
      println(s.mkString("\r\n"))
    }
  }
  
  // TODO: iterator from
  trait Selector {
    def apply(tableau: Array[Array[Rational]]): (Int, Int)
  }
  
  def format(problem: LinearProgram): StandardForm = {
    import LinearProgram._
    val decideSize = problem.varSize
    val consts = problem.consts map {
      const => if (const.constant.isNegative) const.negate() else const
    }
    val constraintSize = consts.length
    val surplusIdx = new Array[Int](constraintSize)
    var surplusSize = 0
    val surplus = new Array[Variable](constraintSize)
    val nonBasic = new Array[Variable](constraintSize)
    View.Range(0, constraintSize).foreach(i => consts(i) match {
      case Constraint(_, <=, _)   => nonBasic(i) = SlackVar(i)
      case Constraint(ai, `=`, _) => nonBasic(i) = ArtifactVar(i)
      case Constraint(ai, >=, _) => {
        nonBasic(i) = ArtifactVar(i)
        surplus(i) = SurplusVar(i)
        surplusIdx(i) = surplusSize
        surplusSize += 1
      }
      case _ => throw new IllegalArgumentException("only >=, <=, = are acceptable")
    })
    val basic = View.Range(0, decideSize).map(ord => DecideVar(ord): Variable) :++ (View.Array(surplus).range(0, surplusSize))
    val basicSize = decideSize + surplusSize
    val c = problem.obj match {
      case Max => problem.coefficients.fill(basicSize, Rational.zero)
      case Min => problem.coefficients.map(_.negate).fill(basicSize, Rational.zero)
    }
    val b = View.Vector(consts).map(_.constant)
    
    val a = View.Range(0, constraintSize).map { i => {
      val const = consts(i)
      const match {
        case Constraint(ai, >=, _) => ai.fill(basicSize, Rational.zero).update(surplusIdx(i) + decideSize, Rational.one.negate)
        case Constraint(ai, _, _) => ai.fill(basicSize, Rational.zero)
      }
    }}
    StandardForm(a, b, c, Rational.zero, View.Array(nonBasic), basic)
  }
}