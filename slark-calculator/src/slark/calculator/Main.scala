package slark.calculator

/**
 * @author a554114
 */
object Main {
  def main(args: Array[String]): Unit = {
    val rule = "<rule>"
    val syntax = "<$syntax>"
    val opt_whitespace = "<opt-whitespace>"
    val rule_name = "<rule-name>"
    val expression = "<expression>"
    val line_end = "<line-end>"
    val list = "<list>"
    val EOL = "<EOL>"
    val term = "<term>"
    val literal = "<literal>"
    val text = "<text>"
    
    println(s"""
               $rule | $rule $syntax
            """)
    println(s"""
               $opt_whitespace "<" $rule_name ">" $opt_whitespace "::=" $opt_whitespace $expression $line_end
            """)
    println(s"""
               " " $opt_whitespace | ""
            """)
    println(s"""
               $list | $list $opt_whitespace "|" $opt_whitespace $expression
            """)
    println(s"""
               $opt_whitespace $EOL | $line_end $line_end
            """)
    println(s"""
               $term | $term $opt_whitespace $list
            """)
    println(s"""
               $literal | "<" $rule_name ">"
            """)
    println(s"""
               '"' $text '"' | "'" $text "'"
            """)
  }
}