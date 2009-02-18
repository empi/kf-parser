package de.istjascha.conti.kf.parser

import org.specs._
import org.specs.matcher._

import de.istjascha.conti.kf.parser._

object functionDefinitionSpec extends Specification("function definition") {
  val matchSuccess = new SuccessMatcher
  val notMatchSuccess = matchSuccess.not
  
  def parse(exp: String) = KFStatementParser parseDfa( exp)
 
  "a function definition parser" should {
	  "accept function definitions" in {    
	     parse( "defun: one(Name $name, Number $gut)@{34;}Any;") must matchSuccess     
	     parse( "defun: two( ) @{23; 42;} Integer;") must matchSuccess     
	  } 
	  
	  
	  "reject illegal function definitions" in {    
	     parse( "defun: eins(Name $name, Number $gut)@{34;};") must notMatchSuccess     
	     parse( "defun: zwei(Name $name, Number $gut)@{34;} Hallo;") must notMatchSuccess  
	     parse( "defun: drei(Name $name, Number if )@{34;} Integer;") must notMatchSuccess
	     parse( "defun: vier(Name $name, Else $if )@{34;} Integer;") must notMatchSuccess
         parse( "defun: 34(Name $name, Instance $foo )@{34;} Integer;") must notMatchSuccess
         parse( "defun: $varname(Name $name )@{34;} Integer;") must notMatchSuccess
         parse( "defun: rulename:(Name $name)@{34;} Integer;") must notMatchSuccess  
	  }
  }
}
