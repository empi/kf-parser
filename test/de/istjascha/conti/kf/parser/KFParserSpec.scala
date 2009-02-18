package de.istjascha.conti.kf.parser

import org.specs._

object KFParserSpec extends Specification {
  "a KF parser".isSpecifiedBy(
                  basicStatementsSpec, functionDefinitionSpec )
}

