package de.istjascha.conti.kf.parser

import de.istjascha.conti.kf.parser.syntactical.KFTokens
import de.istjascha.conti.kf.parser.lexical.KFLexical 

import scala.util.parsing.combinator.syntactical._
class KFParsers extends TokenParsers{
   type Tokens = KFTokens
   val lexical = new KFLexical
   import lexical._;
    /** A parser which matches a single keyword token.
   *
   * @param chars    The character string making up the matched keyword. 
   * @return a `Parser' that matches the given string
   */
  implicit def keyword(chars: String): Parser[String] = accept(Keyword(chars.toLowerCase)) ^^ ("op: " + _.chars)

  /** A parser which matches a numeric literal */
  def numericLit: Parser[String] = 
    elem("number", _.isInstanceOf[NumericLit]) ^^ ("numeric_literal: " + _.chars)

  /** A parser which matches a string literal */
  def stringLit: Parser[String] = 
    elem("string literal", _.isInstanceOf[StringLit]) ^^ ("string_literal: "+ _.chars)

  /** A parser which matches an identifier */
  def name: Parser[String] = 
    elem("name", _.isInstanceOf[Name]) ^^ ("name: "+ _.chars)
  
  
  /** A parser which matches a variable */
  def variable: Parser[String] = 
    elem("name", _.isInstanceOf[Variable]) ^^ ("variable: "+ _.chars)
   
  
  /** A parser which matches a ruleIdentifier */
  def ruleIdentifier: Parser[String] = 
    elem("name", _.isInstanceOf[RuleIdentifier]) ^^ ("rule_id: " + _.chars)
  
    
  /** A parser which matches a ruleIdentifier */
  def refChain: Parser[String] = 
    elem("name", _.isInstanceOf[RefChain ]) ^^ ("ref_chain: " + _.chars)
}
