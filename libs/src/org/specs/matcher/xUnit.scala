package org.specs.matcher
import org.specs.specification._
/**
 * This trait provides methods to help the transition for xUnit users
 */
trait xUnit { self: Specification =>
  def assertTrue(v: => Boolean) = v must beTrue
  def assertFalse(v: => Boolean) = v must beFalse
  def assertEquals[T](a: =>T, b: =>T) = a must_== b
  def assertSame[T](a: =>T, b: =>T) = a mustBe b
  def assertNotSame[T](a: =>T, b: =>T) = a mustNotBe b
  def assertNull[T](a: =>T) = a must beNull
  def assertNotNull[T](a: =>T) = a must notBeNull
  def assertArrayEquals[T](a: =>Array[T], b: =>Array[T]) = a must ((beEqualTo(_:T)).toSeq)(b)
}
