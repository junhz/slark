package slark

import scala.collection.immutable.Traversable

object Diff {
  type Scripts = List[Script]

  trait Algorithm {
    def apply[T](source: Traversable[T], modified: Traversable[T])(isSame: (T, T) => Boolean): Scripts
  }

  sealed trait Script
  case object Insert extends Script
  case object Delete extends Script
  case object Remain extends Script

  def deleteAll[T](source: Traversable[T]): Scripts = {
    @tailrec
    def rec(source: Traversable[T], scripts: Scripts): Scripts = {
      if (source.isEmpty) scripts.reverse
      else rec(source.tail, Delete :: scripts)
    }
    rec(source, Nil)
  }

  def insertAll[T](modified: Traversable[T]): Scripts = {
    @tailrec
    def rec(source: Traversable[T], scripts: Scripts): Scripts = {
      if (source.isEmpty) scripts.reverse
      else rec(source.tail, Insert :: scripts)
    }
    rec(modified, Nil)
  }

  def remainAll[T](source: Traversable[T]): Scripts = {
    @tailrec
    def rec(source: Traversable[T], scripts: Scripts): Scripts = {
      if (source.isEmpty) scripts.reverse
      else rec(source.tail, Remain :: scripts)
    }
    rec(source, Nil)
  }

  /**
   * opt:
   * 1. search path can't cross
   * 2. Remain script count is used to decide further search
   */
  object MyersDiff extends Algorithm {

    case class Draft[T](scripts: Scripts, source: Traversable[T], modified: Traversable[T], unchanged: Int) {
      def canDelete = !source.isEmpty
      def delete: Draft[T] = Draft(Delete :: scripts, source.tail, modified, unchanged)

      def canInsert = !modified.isEmpty
      def insert: Draft[T] = Draft(Insert :: scripts, source, modified.tail, unchanged)

      def isDone = source.isEmpty && modified.isEmpty
    }

    @tailrec
    def findGoodEnough[T](drafts: List[Draft[T]]): Option[Draft[T]] = {
      if (drafts.isEmpty) None
      else if (drafts.head.isDone) Some(drafts.head)
      else findGoodEnough(drafts.tail)
    }

    def better[T](draft1: Draft[T], draft2: Draft[T]): Draft[T] = {
      if (draft1.unchanged > draft2.unchanged) draft1
      else if (draft1.unchanged < draft2.unchanged) draft2
      else draft1
    }

    override def apply[T](source: Traversable[T], modified: Traversable[T])(isSame: (T, T) => Boolean): Scripts = {
      @tailrec
      def rec(toRefine: List[Draft[T]]): Scripts = {
        val nextToRefine = skipGoodPart(edit(toRefine))
        findGoodEnough(nextToRefine) match {
          case None => rec(nextToRefine)
          case Some(draft) => draft.scripts.reverse
        }
      }

      def edit(toEdit: List[Draft[T]]): List[Draft[T]] = {
        @tailrec
        def rec(toEdit: List[Draft[T]], edited: List[Draft[T]]): List[Draft[T]] = {
          if (toEdit.isEmpty) edited
          else if (!toEdit.head.canInsert) better(toEdit.head.delete, edited.head) :: edited.tail
          else if (!toEdit.head.canDelete) rec(toEdit.tail, toEdit.head.insert :: Nil)
          else rec(toEdit.tail, toEdit.head.insert :: better(toEdit.head.delete, edited.head) :: edited.tail)
        }

        if (!toEdit.head.canInsert) toEdit.head.delete :: Nil
        else if (!toEdit.head.canDelete) rec(toEdit.tail, toEdit.head.insert :: Nil)
        else rec(toEdit.tail, toEdit.head.insert :: toEdit.head.delete :: Nil)
      }

      def skipGoodPart(edited: List[Draft[T]]): List[Draft[T]] = {
        @tailrec
        def rec(edited: List[Draft[T]], toEdit: List[Draft[T]]): List[Draft[T]] = {
          if (edited.isEmpty) toEdit
          else rec(edited.tail, nextEditPos(edited.head) :: toEdit)
        }
        rec(edited, Nil)
      }

      @tailrec
      def nextEditPos(draft: Draft[T]): Draft[T] = {
        val Draft(scripts, source, modified, unchanged) = draft
        if (source.isEmpty || modified.isEmpty || !isSame(source.head, modified.head)) draft
        else nextEditPos(Draft(Remain :: scripts, source.tail, modified.tail, unchanged + 1))
      }

      if (source.isEmpty && modified.isEmpty) Nil
      else if (source.isEmpty) insertAll(modified)
      else if (modified.isEmpty) deleteAll(source)
      else {
        val draft = nextEditPos(Draft(Nil, source, modified, 0))
        if (draft.isDone) draft.scripts.reverse
        else rec(draft :: Nil)
      }
    }
  }
}