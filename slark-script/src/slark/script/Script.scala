package slark.script

trait Script {

  def apply(args: String*): Any
  
}