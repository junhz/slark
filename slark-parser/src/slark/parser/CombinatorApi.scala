package slark
package parser

trait CombinatorApi { self: Parsers =>

  sealed trait OptResult
  case class End(result: ParseResult[Any]) extends OptResult
  case class Next(results: List[ParseResult[Any]], ops: List[Operation]) extends OptResult

  trait Operation {
    def apply(results: List[ParseResult[Any]], ops: List[Operation]): OptResult
  }

  case class Eval(parser: Parser[Any]) extends Operation {
    def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case f: Fail => End(results.head)
        case Succ(r, n) => Next(parser.parse(n) :: results, ops)
      }
    }

    override def toString = parser.toString()
  }

  case class CombinedParser[+S](ops: List[Operation]) extends Parser[S] {
    override def parse(input: Input) = {

      @tailrec
      def rec(rss: List[List[ParseResult[Any]]], opss: List[List[Operation]]): ParseResult[S] = {
        if (opss.isEmpty) {
          assert(rss.tail.isEmpty && rss.head.tail.isEmpty)
          rss.head.head.asInstanceOf[ParseResult[S]]
        } else if (opss.head.isEmpty) {
          rec((rss.head.head :: rss.tail.head) :: rss.tail.tail, opss.tail)
        } else {
          opss.head.head match {
            case Eval(CombinedParser(ops)) => {
              rss.head.head match {
                case f: Fail => rec((f :: Nil) :: rss.tail, Nil :: opss.tail)
                case _ => rec((rss.head.head :: Nil) :: rss, ops :: opss.head.tail :: opss.tail)
              }
            }
            case op => op(rss.head, opss.head.tail) match {
              case End(r) => rec((r :: Nil) :: rss.tail, Nil :: opss.tail)
              case Next(rs, ops) => rec(rs :: rss.tail, ops :: opss.tail)
            }
          }
        }
      }

      rec(((Succ((), input) :: Nil) :: Nil :: Nil), ops :: Nil)
    }

    override def toString = "("+ops.mkString(" ")+")"
  }

  case object Alt extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case f: Fail => Next(results.tail, ops)
        case s => End(s)
      }
    }

    override def toString = "|"
  }

  def alt[S, S_ >: S](lhs: Parser[S], rhs: Parser[S_]): Parser[S_] = CombinedParser[S_](Eval(lhs) :: Alt :: Eval(rhs) :: Nil)

  case object MakeList extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case Succ(_, n) => Next((Succ(Nil, n) :: Nil), ops)
        case _ => throw new AssertionError
      }
    }

    override def toString = "List"
  }

  case class Cons(count: Int, endWhenSucc: Int => Boolean, liveWithFail: Int => Boolean) extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case Succ(r, n) => results.tail.head match {
          case Succ(l: List[Any], _) =>
            if (endWhenSucc(count + 1)) End(Succ((r :: l).reverse, n))
            else Next(Succ(r :: l, n) :: Nil, ops.head :: Cons(count + 1, endWhenSucc, liveWithFail) :: ops)
          case _ => throw new AssertionError
        }
        case Fail(msg) =>
          if (liveWithFail(count)) results.tail.head match {
            case Succ(l: List[Any], n) => End(Succ(l.reverse, n))
            case _ => throw new AssertionError
          }
          else End(Fail(s"failed at ${count + 1} attemp: $msg"))
      }
    }
  }

  def rep[S](p: Parser[S])(endWhenSucc: Int => Boolean)(liveWithFail: Int => Boolean): Parser[List[S]] = {
    CombinedParser[List[S]](MakeList :: Eval(p) :: Cons(0, endWhenSucc, liveWithFail) :: Eval(p) :: Nil)
  }

  case object Pair extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case f: Fail => End(f)
        case Succ(r2, n) => results.tail.head match {
          case Succ(r1, _) => End(Succ((r1, r2), n))
          case _ => throw new AssertionError
        }
      }
    }
  }

  def seq[S, S_](lhs: Parser[S], rhs: Parser[S_]): Parser[(S, S_)] = CombinedParser[(S, S_)](Eval(lhs) :: Eval(rhs) :: Pair :: Nil)

  case class Map(fn: Any => Any) extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case f: Fail => End(results.head)
        case Succ(r, n) => {
          val result = try {
            Succ(fn(r), n)
          } catch {
            case e: Throwable => Fail(e.getMessage())
          }
          End(result)
        }
      }
    }

    override def toString = s"-> ${fn.getClass().getName()}"
  }

  def map[S, S_](p: Parser[S])(fn: S => S_): Parser[S_] = CombinedParser[S_](Eval(p) :: Map(fn.asInstanceOf[Any => Any]) :: Nil)

  case class Flatmap(fn: Any => Parser[Any]) extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case f: Fail => End(f)
        case Succ(r, n) => {
          try {
            Next(Succ(r, n) :: Nil, Eval(fn(r)) :: Nil)
          } catch {
            case e: Throwable => End(Fail(e.getMessage()))
          }
        }
      }
    }

    override def toString = s">> ${fn.getClass().getName()}"
  }

  def flatmap[S, S_](p: Parser[S])(fn: S => Parser[S_]): Parser[S_] =
    CombinedParser[S_](Eval(p) :: Flatmap(fn.asInstanceOf[Any => Parser[Any]]) :: Nil)

  case object Not extends Operation {
    override def apply(results: List[ParseResult[Any]], ops: List[Operation]) = {
      results.head match {
        case f: Fail => results.tail.head match {
          case Succ(_, n) => End(Succ((), n))
          case _ => throw new AssertionError
        }
        case Succ(n, _) => End(Fail(s"$n is not acceptable input"))
      }
    }

    override def toString = "!"
  }

  def not(p: Parser[_]): Parser[Unit] = CombinedParser[Unit](Eval(p) :: Not :: Nil)
}