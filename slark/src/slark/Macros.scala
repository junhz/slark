package slark

object Macros {

  import scala.reflect.macros.Context
  
  def searchForDef(c: Context)(root: c.universe.Tree, target: c.universe.Tree): List[c.universe.Tree] = {
    import c.universe._
    
    @tailrec
      def rec(path: List[Tree], bodies: List[List[Tree]]): List[Tree] = {
        if (bodies.isEmpty) List()
        else if (bodies.head.isEmpty) rec(path.tail, bodies.tail)
        else if (bodies.head.head == target) (target :: path).reverse
        else {
          bodies.head.head match {
            case EmptyTree => rec(path, bodies.head.tail :: bodies.tail)
            case _: Import => rec(path, bodies.head.tail :: bodies.tail)
            case _: Super => rec(path, bodies.head.tail :: bodies.tail)
            case _: Literal => rec(path, bodies.head.tail :: bodies.tail)
            case _: Ident => rec(path, bodies.head.tail :: bodies.tail)
            case _: TypeBoundsTree => rec(path, bodies.head.tail :: bodies.tail)
            case _: TypeDef => rec(path, bodies.head.tail :: bodies.tail)
            case _: This => rec(path, bodies.head.tail :: bodies.tail)
            case t @ ModuleDef(_, _, impl) => rec(t :: path, impl.body :: bodies.head.tail :: bodies.tail)
            case t @ ClassDef(_, _, _, impl) => rec(t :: path, impl.body :: bodies.head.tail :: bodies.tail)
            case t @ PackageDef(_, stats) => rec(t :: path, stats :: bodies.head.tail :: bodies.tail)
            case t @ DefDef(_, _, _, _, _, rhs) => rec(t :: path, (rhs :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ ValDef(_, _, _, rhs) => rec(t :: path, (rhs :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ New(tpt) => rec(t :: path, (tpt :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ Apply(fun, args) => rec(t :: path, (fun :: args) :: bodies.head.tail :: bodies.tail)
            case t @ Block(stats, expr) => rec(t :: path, (expr :: stats) :: bodies.head.tail :: bodies.tail)
            case t @ Select(qualifier, _) => rec(t :: path, (qualifier :: Nil) :: bodies.head.tail :: bodies.tail)
            case t @ Template(_, _, body) => rec(t :: path, body :: bodies.head.tail :: bodies.tail)
            case t @ Function(_, impl) => rec(t :: path, (impl :: Nil) :: bodies.head.tail :: bodies.tail)
            case tree => {
              c.warning(tree.pos, s"${tree.getClass().getName()} not matched: ${tree.toString}")
              rec(tree :: path, tree.children :: bodies.head.tail :: bodies.tail)
            }
          }
        }
      }

      rec(Nil, (root :: Nil) :: Nil)
  }
  
}