package slark.mirage

/**
 * @author a554114
 */
trait Knowledge {
  
}

object Knowledge {
  case class Column[T](name: String, dataType: db.DataType[T], nullable: Boolean) extends Knowledge
  
  case class Index(name: String, unique: Boolean, columns: List[String]) extends Knowledge
  
  case class Table(owner: String, name: String, columns: List[Column[_]], indexes: List[Index], avgLen: Int) extends Knowledge
  
  case class Function(owner: String, name: String, source: String) extends Knowledge
  
  case class Procedure(owner: String, name: String, source: String) extends Knowledge
  
  case class View(owner: String, name: String, source: String) extends Knowledge
  
  case class Synonym(owner: String, name: String, targetOwner: String, targetName: String) extends Knowledge
  
  case class Package(owner: String, name: String, children: List[String], visible: Map[String, (String, String)], hidden: Map[String, (String, String)]) extends Knowledge
  
  case class Query(sql: String) extends Knowledge
  
  case class ObjectType(owner: String, name: String, ref: Option[Mirage]) extends Knowledge
  
  case class `Type`(owner: String, name: String, source: String) extends Knowledge
  
  case class Sequence(owner: String, name: String, start: Long, ordered: Boolean) extends Knowledge
}