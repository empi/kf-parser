package org.specs.specification
import org.specs.matcher._

/**
 * This trait defines implicit definitions which are used to create Expectable objects
 * and associate them with the latest defined example<br>
 * Usage: <code>
 * 2.1 must beCloseTo(2.0, .1)
 * </code>or<br><code>
 * theDouble(2.1) must beCloseTo(2.0, .1)
 * </code><p> 
 * 
 * Then StringMatchers are expecting values with String as an upper bound so that effectively only String instances
 * will be used with the implicit def (only solution found to make it all work, to my current understanding)
 */
trait ExpectableFactory extends ExampleExpectationsListener with SuccessValues {
  
  /** implicit transformation of an object into one supporting AnyMatcher matchers */
  implicit def theValue[A](value: =>A): Expectation[A] = {
    val a = new Expectation(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(this)
  }

  /** implicit transformation of a String into an object supporting String matchers */
  implicit def theString[A >: String](value: =>A) = {
    val a = new StringExpectable(value.toString)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(this)
  }

  /** 
   * implicit transformation of a block returning Nothing. 
   * This is necessary when testing thrown exceptions <pre>stream.close must throwA(new IOException)</pre> 
   */
  implicit def theBlock(value: =>Nothing): Expectation[Nothing] = {
    val a = new Expectation(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(this)
  }
  /** implicit transformation of an Iterable[String] into an object supporting IterableString matchers */
  implicit def theStrings(value: =>Iterable[String]): IterableStringExpectable = {
    val a = new IterableStringExpectable(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(this)
  }

  /** implicit transformation of an Iterable into an object supporting Iterable matchers */
  implicit def theIterable[I <: AnyRef](value: =>Iterable[I]): IterableExpectable[I] = {
    val a = new IterableExpectable(value)
    a.setSuccessValueToString(successValueToString _)
    a.setExpectationsListener(this)
  }
}
