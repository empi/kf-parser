package de.istjascha.conti.kf.parser

import collection.mutable.HashSet
object KFStatementParser extends KFParsers {
   
  
  def parseDfa(exp: String) =
   {
     val tokens = new lexical.Scanner(exp)
     phrase(dfaFile)(tokens)
   }
  
   def parseStatements(exp: String) =
   {
     val tokens = new lexical.Scanner(exp)
     phrase(statementList)(tokens)
   }
  
  
  lexical.delimiters += ("@{", "{", "}", "[", "]", ";", ",", "<<", "(", ")")
  lexical.reserved += ( "true", "false", "method", "defun:", "defclass:", "child", "childList", "if", "then", "else", "for", "in", "is", "loop", "do", "from", "to", "collect", "append", "then")
  
  val types = new HashSet[String]{
    override def +=(elem: String) { addEntry(elem.toLowerCase) } 
    override def -=(elem: String) { removeEntry(elem.toLowerCase) }
  }
  types += ("any", "integer",  "number", "user", "list", "string", "boolean", "Point", "Vector", "Instance", "frame", "name")  
  
  val flags = "parameter" :: "canonical" :: "uncached" :: "cached" :: "method" :: Nil
  
  lexical.reserved ++= types
  lexical.reserved ++= flags
  
  def statementList : Parser[Any] =
    rep1(statement)
   
  def statement : Parser[Any] =
    ( variable_assignment | value) ~ ";" 
  
  def variable_assignment : Parser[Any] =
    variable ~ "<<" ~ value
  
  def value : Parser[Any] =
    executableBlock|  methCall| functionCall | loop | literal 
  
  def literal =
    numericLit | variable | stringLit | "true" | "false" | 
    name  |  list  |  ruleIdentifier | refChain 
  
  def executableBlock = "@{" ~ statementList ~ "}"
  
  def list   : Parser[Any] = 
    "{" ~  value  ~ "}" |
    "{" ~  repsep(value, ",") ~ optComma   ~ "}"
  
  def ifThenElse =
    "if" ~"("~value ~ ")"  

  def optComma = opt(",") 
  def methCall = ruleIdentifier ~ "(" ~ repsep(value, ",") ~ ")"
  def functionCall = name ~ "(" ~ repsep(value, ",")~ optComma ~ ")"
  
  def loopForIn =
    "for" ~ variable ~ "in" ~ value ~ ";"
  
  def loopForIs =
    "for" ~ variable ~ "is" ~ value ~ ";"
  
  def loopFromTo =
    "for" ~ variable ~ "from" ~ numericLit ~ "to" ~ numericLit ~ ";"
  
  def loopForIsThen =
    "for" ~ variable ~ "is" ~ value ~ "then"~ value~";"
  
  def loopInitializer =
    loopForIn | loopFromTo | loopForIsThen
  
  def loopInitializers =
     rep1( loopInitializer )
  
  def loopAssignments =
    rep( loopForIs  )
  
  def loop =
    "loop" ~ "{" ~ loopBody ~ "}"
  
  def returnIs =
    "return" ~ "is" ~ value ~";"
  def loopDo =
    "do" ~ statement;
  
  def loopCollector =
    collect| append | loopDo
   
  def loopCollectors =
    rep1(loopCollector)
  
  def collect =
    "collect" ~value ~ ";"
  
  def append =
    "append" ~value ~ ";"
  def loopBody =
    loopInitializers ~ loopAssignments ~ loopCollectors 

   def classDefinition =
    "defclass:" ~ name ~ "("  ~ repsep(name, ",") ~")" ~ ";" 
   
   def ruleDefinition = 
     "(" ~ rep(flag) ~ typeName ~ rep(flag) ~ ")" ~ ruleIdentifier ~ value ~";"
  
  def functionDefinition = "defun:" ~ name ~"("  ~ repsep(typeName ~ variable, ",") ~")" ~
    executableBlock ~ typeName~";"
   

     
  def typeName = types.map(keyword).reduceRight[Parser[String]](_ | _) // pipe em together
  def flag = flags.map(keyword).reduceRight[Parser[String]](_ | _)
  
  def dfaFile =
    rep1( classDefinition | functionDefinition |ruleDefinition )
  
  def main(args: Array[String]) {
    val exp = "{};";
    val tokens = new lexical.Scanner(exp)
    println("\""+exp+"\"")
    println(phrase(dfaFile)(tokens))    
  }
}
