package examples

import scala.util.parsing.combinator.syntactical.StandardTokenParsers

object JSON extends StandardTokenParsers {   
  lexical.delimiters += ("{", "}", "[", "]", ":", ",")
  lexical.reserved += ("null", "true", "false")

  def obj   : Parser[Any] = "{" ~ repsep(member, ",") ~ "}"
  def arr   : Parser[Any] = "[" ~ repsep(value, ",") ~ "]"
  def member: Parser[Any] = ident ~ ":" ~ value
  def value : Parser[Any] = ident | numericLit | obj | arr | 
                            "null" | "true" | "false"

  def main(args: Array[String]) {
    val exp ="true"
    val tokens = new lexical.Scanner(exp)
    println(exp)
    println(phrase(value)(tokens))
  }
}
object JSON1 extends StandardTokenParsers {   
  lexical.delimiters += ("{", "}", "[", "]", ":", ",")
  lexical.reserved += ("null", "true", "false")

  def obj: Parser[Map[String, Any]] = 
    "{" ~> repsep(member, ",") <~ "}" ^^ (Map() ++ _)

  def arr: Parser[List[Any]] =
    "[" ~> repsep(value, ",") <~ "]" 

  def member: Parser[(String, Any)] = 
    ident ~ ":" ~ value ^^ { case name ~ ":" ~ value => (name -> value) }

  def value: Parser[Any] = 
    ident | numericLit ^^ (_.toInt) | obj | arr |
    "null" ^^^ null | "true" ^^^ true | "false" ^^^ false

  def main(args: Array[String]) {
    val tokens = new lexical.Scanner(args(0))
    println(args(0))
    println(phrase(value)(tokens))
  }
}

