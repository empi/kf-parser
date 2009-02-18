package de.istjascha.conti.kf.parser

import org.specs.matcher._
import de.istjascha.conti.kf.parser._
class SuccessMatcher extends Matcher[KFStatementParser.ParseResult[Any]]{
 
  def apply(v: => KFStatementParser.ParseResult[Any]) =   
  {    
	  var _tree : Any = null
	  var _msg : Any = null
	  ( 
	     v match { 
	        case KFStatementParser.Success(tree, _) => _tree = tree; true         
	        case e: KFStatementParser.NoSuccess => _msg = e; false
	      },
	     "parsed successfully as " + _tree, 
	     "failed to parse. msg: " + _msg
	   )
  }
}
