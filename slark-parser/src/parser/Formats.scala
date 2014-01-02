package slark
package parser

trait Formats { self: Parsers =>

  def __[T]: (T => T) = { t => t }

  def ?[S](default: S): Option[S] => S = _ match {
    case None => default
    case Some(s) => s
  }

  def ^[L, L_, R, R_](lhs: (L => L_), rhs: (R => R_)): ((L, R)) => (L_, R_) = _ match {
    case (l, r) => (lhs(l), rhs(r))
  }

  private[this] def *[S](list: List[S], extractors: Any*): List[Any] = {
    @tailrec
    def rec(extractors: Seq[Any], list: List[S], results: List[Any]): List[Any] = {
      if (extractors.isEmpty) {
        if (list.isEmpty) results.reverse
        else Nil
      } else {
        val extractor = extractors.head.asInstanceOf[Option[S] => Any]
        val r = try {
          Left(extractor(list.headOption))
        } catch {
          case e: Throwable => {
            Right(e.getMessage())
          }
        }

        r match {
          case Left(r) => rec(extractors.tail, if (list.isEmpty) Nil else list.tail, r :: results)
          case Right(msg) => Nil
        }
      }
    }

    rec(extractors, list, Nil)
  }

  def *[S, S_](ex: (Option[S] => S_)): (List[S] => S_) = *(_, ex) match {
    case List(r) => r.asInstanceOf[S_]
  }

  def *[S, S1, S2](ex1: (Option[S] => S1), ex2: (Option[S] => S2)): (List[S] => (S1, S2)) = *(_, ex1, ex2) match {
    case List(r1, r2) => (r1, r2).asInstanceOf[(S1, S2)]
  }

  def *[S, S1, S2, S3](ex1: (Option[S] => S1), ex2: (Option[S] => S2), ex3: (Option[S] => S3)): (List[S] => (S1, S2, S3)) =
    *(_, ex1, ex2, ex3) match {
      case List(r1, r2, r3) => (r1, r2, r3).asInstanceOf[(S1, S2, S3)]
    }

  def *[S, S1, S2, S3, S4](ex1: (Option[S] => S1), ex2: (Option[S] => S2), ex3: (Option[S] => S3), ex4: (Option[S] => S4)): (List[S] => (S1, S2, S3, S4)) =
    *(_, ex1, ex2, ex3, ex4) match {
      case List(r1, r2, r3, r4) => (r1, r2, r3, r4).asInstanceOf[(S1, S2, S3, S4)]
    }

  def *[S, S1, S2, S3, S4, S5](ex1: (Option[S] => S1), ex2: (Option[S] => S2), ex3: (Option[S] => S3), ex4: (Option[S] => S4), ex5: (Option[S] => S5)): (List[S] => (S1, S2, S3, S4, S5)) =
    *(_, ex1, ex2, ex3, ex4, ex5) match {
      case List(r1, r2, r3, r4, r5) => (r1, r2, r3, r4, r5).asInstanceOf[(S1, S2, S3, S4, S5)]
    }

  def *[S, S1, S2, S3, S4, S5, S6](ex1: (Option[S] => S1), ex2: (Option[S] => S2), ex3: (Option[S] => S3), ex4: (Option[S] => S4), ex5: (Option[S] => S5), ex6: (Option[S] => S6)): (List[S] => (S1, S2, S3, S4, S5, S6)) =
    *(_, ex1, ex2, ex3, ex4, ex5, ex6) match {
      case List(r1, r2, r3, r4, r5, r6) => (r1, r2, r3, r4, r5, r6).asInstanceOf[(S1, S2, S3, S4, S5, S6)]
    }

}