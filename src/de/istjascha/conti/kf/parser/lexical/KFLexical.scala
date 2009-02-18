package de.istjascha.conti.kf.parser.lexical

import scala.util.parsing.combinator.lexical.Lexical;
import scala.util.parsing.syntax._;
import scala.util.parsing.input.CharArrayReader.EofCh
import collection.mutable.HashSet

import syntactical.KFTokens
import de.istjascha.util.LowerCaseHashSetString

class KFLexical extends Lexical with KFTokens{
 
  def token: Parser[Token] = 
    (
      kfDef
    | kfRefChain
    | kfRuleIdentifier 				    
    | kfVariable        
    | kfName
    | numericVal
    | '\'' ~ rep( chrExcept('\'', '\n', EofCh) ) ~ '\'' ^^ { case '\'' ~ chars ~ '\'' => StringLit(chars mkString "") }
    | '\"' ~ rep( chrExcept('\"', '\n', EofCh) ) ~ '\"' ^^ { case '\"' ~ chars ~ '\"' => StringLit(chars mkString "") }
    | EofCh                                             ^^^ EOF
    | '\'' ~> failure("unclosed string literal")        
    | '\"' ~> failure("unclosed string literal")        
    | delim                                             
    | failure("illegal character")
    )
    // see `whitespace in `Scanners'
  def whitespace: Parser[Any] = rep(
      whitespaceChar
    | '#' ~ '+' ~ comment
    | '#'  ~ rep( chrExcept(EofCh, '\n') )
    | '#' ~ '+' ~ failure("unclosed comment")
    )
   
  def numericVal = 
    ( floatingToken 								    ^^ { case sign ~ intPart ~ frac ~ exp => NumericLit( sign :: (intPart mkString "") :: frac :: exp :: Nil mkString "")}
    | optSign ~  digit ~ rep( digit )                   ^^ { case sign ~ int ~ rest => NumericLit(sign :: digit :: rest mkString "") } 
    ) 

  protected def comment: Parser[Any] = (
      '#' ~ '-'  ^^ { case _ => ' '  }
    | chrExcept(EofCh) ~ comment
    )
  
  def kfIdentifier = 
  ( letter|chr('%')|chr('?')|chr('_') )~  rep( letter | digit |chr('_')|chr('%')|chr('?')) ^^ { case first ~ rest =>first :: rest mkString "" }
  
  def kfName = kfIdentifier  ^^ {case name => processKeywordOrName(name)}
  
  def kfRuleIdentifier = kfIdentifier ~ chr(':')  ^^ { case first ~ rest => RuleIdentifier(  first + rest) }
  
  def kfRefChain = kfRuleIdentifier ~  rep1(kfRuleIdentifier) ^^
    {case first ~ second  => RefChain( first :: second )}
  
  def kfVariable = chr('$' ) ~ kfIdentifier ^^ { case dollar ~ id => Variable( dollar + id ) }
 
  def kfDef	= (parseDelim("defun") | parseDelim("defclass")) ~ chr(':') ^^ {case definition ~ rest => Keyword(definition.chars + rest.charValue)}
  
  /** The set of delimiters (ordering does not matter) */
  val delimiters = new HashSet[String]

    /** The set of reserved identifiers: these will be returned as `Keyword's */
  val reserved = new HashSet[String]{
    override def +=(elem: String) { addEntry(elem.toLowerCase) } 
    override def -=(elem: String) { removeEntry(elem.toLowerCase) }
  }

  
  protected def processKeywordOrName(name: String) = {   
    val _name = name.toLowerCase
    if (reserved contains _name) Keyword(_name) else Name(name)
  } 
  def parseDelim(s: String): Parser[Token] = accept(s.toList) ^^ { x => Keyword(s) }
  
  private var _delim: Parser[Token] = null
  protected def delim: Parser[Token] = {
    if (_delim eq null) { // construct parser for delimiters by |'ing together the parsers for the individual delimiters, 
    // starting with the longest one (hence the sort + reverse) -- otherwise a delimiter D will never be matched if 
    // there is another delimiter that is a prefix of D   
      
      
      val d = new Array[String](delimiters.size)
      delimiters.copyToArray(d,0)
      scala.util.Sorting.quickSort(d) 
      _delim = d.toList.reverse.map(parseDelim).reduceRight[Parser[Token]](_ | _) // no offence :-)      
    }
    
    _delim
  }
  
   def floatingToken =
        optSign ~ rep1(digit) ~ optFraction ~ optExponent 
    
   def optFraction = opt(fraction) ^^ {
        case None => ""
        case Some(fraction) => fraction
    }
    def fraction = '.' ~ rep(digit) ^^ {
        case dot ~ ff => dot :: (ff mkString "") :: Nil mkString ""
    }

    def exponent = (chr('e') | chr('E')) ~ optSign ~ rep1(digit) ^^ {
        case e ~ optSign ~ exp => e :: optSign :: (exp mkString "") :: Nil mkString ""
    }
      
    def optExponent = opt(exponent) ^^ {
        case None => ""
        case Some(exponent) => exponent
    }
    def chr(c:Char) = elem("", ch => ch==c )
    def sign = chr('+') | chr('-')
    def optSign = opt(sign) ^^ {
        case None => ""
        case Some(sign) => sign
    }

}


