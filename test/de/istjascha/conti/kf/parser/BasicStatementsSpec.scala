package de.istjascha.conti.kf.parser

import org.specs._
import org.specs.matcher._

import de.istjascha.conti.kf.parser._

object basicStatementsSpec extends Specification("basic statements") {  
  
  val matchSuccess = new SuccessMatcher
  val notMatchSuccess = matchSuccess.not
  
  def parse(exp: String) = KFStatementParser parseStatements( exp)   
  
  "a statement parser" should {  
      "have a sensible delimiters and keyword list" in
        {
          def stringWithUpperCase( str: String) =
            str != str.toLowerCase
          
          KFStatementParser.lexical.delimiters exists( _.contains(" ")) must beFalse          
          KFStatementParser.lexical.reserved   exists( _.contains(" ")) must beFalse        
          KFStatementParser.lexical.delimiters.size must notBe( 0 )
          KFStatementParser.lexical.reserved.  size must notBe( 0 )
          KFStatementParser.lexical.delimiters exists( stringWithUpperCase ) must beFalse
          KFStatementParser.lexical.reserved   exists( stringWithUpperCase ) must beFalse
        }
	  "reject empty statements" in {    
	     parse( "@{};") must notMatchSuccess
	     parse( "@{abc};") must notMatchSuccess
	     parse( "@{abc:};") must notMatchSuccess
	     parse( ";") must notMatchSuccess
	     parse( "") must notMatchSuccess
	     parse( ";;") must notMatchSuccess
	     parse( "loop{};") must notMatchSuccess
	     parse( "loop{23;};") must notMatchSuccess
	     parse( "23 + ;") must notMatchSuccess
	     parse( "\";") must notMatchSuccess
	     parse( "\"") must notMatchSuccess
	     parse( ",") must notMatchSuccess 
	  }
	  
	  "accept an empty list" in {    
	     parse( "{};") must matchSuccess          
	  }
	  
	  "accept an empty list with comma" in {    
	     parse( "{,};") must matchSuccess          
	  }
	  
	  "reject an empty list with 2 commas" in {    
	     parse( "{,,};") must notMatchSuccess        
	  }
	  
	  "accept a function call" in {    
	     parse( "foo();") must matchSuccess     
	     parse( "foo( );") must matchSuccess
	     parse( "foo( , );") must matchSuccess
	  }
	  
	  "accept a function call with parameters" in {    
	     parse( "one(34);") must matchSuccess     
	     parse( "two( {,}, );") must matchSuccess
	     parse( "three( $%sauber );") must matchSuccess
	     parse( "four(@{p;});") must matchSuccess
	     parse( "five(gut);") must matchSuccess
	  } 
	  
	  "reject a function call with illegal parameters" in {    
	     parse( "eins(34 ist gut);") must notMatchSuccess     
	     parse( "zwei( {,$}, );") must notMatchSuccess
	     parse( "drei( $%sauber+ );") must notMatchSuccess
	  }
	  
	  
	  "accept numeric values" in {
	    parse( "34;")  must matchSuccess
	    parse( "-34;")  must matchSuccess
	    parse( "+34;")  must matchSuccess
	    parse( "1.87;")  must matchSuccess
	    parse( "-1.87;")  must matchSuccess
	  }
	  
	  "accept rulenames" in {
	    parse( "%ug_final:;")  must matchSuccess 
        parse( "??ug_final:;")  must matchSuccess
	    parse( "%ug_final__2:;")  must matchSuccess 
        parse( "ug_final?__2:;")  must matchSuccess
        parse( "ug_final?:;")  must matchSuccess
	    parse( "swd_final__2_22_alt:;")  must matchSuccess
	    parse( "%ug_final__2:foobar:;")  must matchSuccess
	    parse( "ug_final__2:foobar:my_funny_rule:;")  must matchSuccess 
        parse( "___:foobar:my_funny_rule:;")  must matchSuccess
	  }
	  
	  "reject illegal rulenames" in {    
	     parse( "%$hallo:;") must notMatchSuccess     
	     parse( "2:;") must notMatchSuccess  
         parse( "2_:;") must notMatchSuccess 
	     parse( ":ba:;") must notMatchSuccess
	     parse( "ba::;") must notMatchSuccess
	     parse( ":;") must notMatchSuccess
	  }
	  
	  
	  "ignore comments" in {
	    parse( "#34;\n 23;")  must matchSuccess
	    parse( "#+ #--34;")  must matchSuccess
	    parse( "#+ \" ~ #--34;")  must matchSuccess
	    parse( "34#+ \" ~ #- ;")  must matchSuccess
	    parse( "34#+ \" \n \n~ #- ;")  must matchSuccess
	    parse( "+34;###")  must matchSuccess
	    parse( "#\n1.87;")  must matchSuccess
	    parse( "\n#\n-1.87;")  must matchSuccess
	    parse( "\"#+  #- \";")  must matchSuccess
        parse( "\"#+ .  #bla   #- \";\n #bla")  must matchSuccess
        parse( "#+.    #- \n24;")  must matchSuccess
	  }
	  
	  "reject open comments" in {
	    parse( "#34;\n")  must notMatchSuccess
	    parse( "#+ # --34;")  must notMatchSuccess
	    parse( "#+ #+ #--  34;")  must notMatchSuccess
	  }
	  
	  "reject open string literals" in {
	    parse( "\"")  must notMatchSuccess
	    parse( "\"\"\";")  must notMatchSuccess
	    parse( "23\";")  must notMatchSuccess
        parse( "\" ~\";\"")  must notMatchSuccess
        parse( "\"\n\";")  must notMatchSuccess  
	  }
   
     "accept string literals" in {
	    parse( "\"\";")  must matchSuccess
        parse( "\" \";")  must matchSuccess	
        parse( "\"34\";")  must matchSuccess
        parse( "\"$d\";")  must matchSuccess
        parse( "\"@ @{\";")  must matchSuccess
        parse( "\"~n\";")  must matchSuccess	
	  }
     
     "accept variables" in {
	    parse( "$foobar;")  must matchSuccess
        parse( "$_foobar_;")  must matchSuccess
        parse( "$%foobar_;")  must matchSuccess
        parse( "$?%foo2?bar_;")  must matchSuccess
	  }
     
     "accept simple loops" in {
	    parse( "loop{for $i in {1,2,3}; do nothing;};")  must matchSuccess
        parse( "loop{for $i is {1,2,3} then 24; do $nothing;};")  must matchSuccess     
	  }
     
     "reject wrong loops" in {
	    parse( "loop{for $i if {1,2,3}; do nothing;};")  must notMatchSuccess
        parse( "loop{for $i in {1,2,3}; do nothing,};")  must notMatchSuccess
        parse( "loop{for $i in {1,2,3,}, do nothing;};")  must notMatchSuccess 
	  }
    }
}