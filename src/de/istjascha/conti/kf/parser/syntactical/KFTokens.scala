package de.istjascha.conti.kf.parser.syntactical
import scala.util.parsing.syntax.Tokens;
trait KFTokens extends Tokens{
  /** The class of keyword tokens */
  case class Keyword(chars: String) extends Token {
    override def toString = "`"+chars+"'"
  }

  /** The class of numeric literal tokens */
  case class NumericLit(chars: String) extends Token {
    override def toString = chars
  }

  /** The class of string literal tokens */
  case class StringLit(chars: String) extends Token {
    override def toString = "\""+chars+"\""
  }    

  /** The class of name tokens */
  case class Name(chars: String) extends Token {
    override def toString = "name "+chars
  }
  
    /** The class of variable tokens */
  case class Variable(chars: String) extends Token {
    override def toString = chars
  }
  
      /** The class of RuleIdentifier tokens */
  case class RuleIdentifier(chars: String) extends Token {
    override def toString = chars
  }
  
       /** The class of RuleIdentifier tokens (refchains)*/
  case class RefChain(rules: List[RuleIdentifier]) extends Token {
    override def toString = "refchain " +  chars
    override def chars = rules mkString ""
  }
}
