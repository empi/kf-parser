package org.specs.collection
import org.specs.collection.ExtendedList._
import scala.collection.mutable.ListBuffer

/**
 * The ExtendedIterable object offers utility methods applicable to iterables like:<ul>
 * <li><code>toStream</code> 
 * <li><code>toDeepString</code>: calls toString recursively on the iterable elements
 * <li><code>sameElementsAs</code>: compares 2 iterables recursively
 * </ul>
 */
object ExtendedIterable {
  /**
   * implicit definition to transform an iterable to an ExtendedIterable
   */
  implicit def iterableToExtended[A](xs : Iterable[A]) = new ExtendedIterable(xs)

  /**
   * See the description of the ExtendedIterable object
   */
  class ExtendedIterable[A](xs:Iterable[A]) {
    /**
     * @return a Stream created from the iterable
     */
    def toStream = Stream.fromIterator(xs.elements)

    /**
     * alias for any type of Iterable
     */
    type anyIterable = Iterable[T] forSome {type T} 
    
    /**
     * @return the representation of the elements of the iterable using the toString method recursively
     */
    def toDeepString: String = {
      if (!xs.isEmpty && xs == xs.elements.next)
        xs.toString
      else
          "[" + xs.toList.map { x =>
            if (x.isInstanceOf[anyIterable]) x.asInstanceOf[anyIterable].toDeepString else x.toString
          }.mkString(", ") + "]" 
    }
    
    /**
     * @return true if the 2 iterables contain the same elements, in the same order, according to a function f 
     */
    def isSimilar[B >: A](that: Iterable[B], f: Function2[A, B, Boolean]): Boolean = {
      val ita = xs.elements
      val itb = that.elements
      var res = true
      while (res && ita.hasNext && itb.hasNext) {
        res = f(ita.next, itb.next)
      }
      !ita.hasNext && !itb.hasNext && res
    }
    /**
     * @return true if the second iterable elements are contained in the first, in order 
     */
    def containsInOrder[A](l: Iterable[A]) = {
        val indexes: List[Int] = l.foldLeft(new ListBuffer[Int]()) { (ind, x) => ind.append(xs.toSeq.findIndexOf(x == _)); ind }.toList
        (!indexes.contains(-1) && indexes.sort(_ <= _) == indexes)
    } 

    /**
     * @return true if the 2 iterables contain the same elements recursively, in any order 
     */
    def sameElementsAs(that: Iterable[A]): Boolean = sameElementsAs(that, (x, y) => x == y)

    /**
     * @return true if the 2 iterables contain the same elements (according to a comparision function f) recursively, in any order 
     */
    def sameElementsAs(that: Iterable[A], f: (A, A) => Boolean): Boolean = {
      def isNotItsOwnIterable(a: Iterable[_]) = a.isEmpty || a.elements.next != a
	  def matchTwo(x: A, y: A): Boolean = {
		(x, y) match {
		  case (a: Iterable[_], b:Iterable[_]) if (isNotItsOwnIterable(a)) => x.asInstanceOf[Iterable[A]].sameElementsAs(y.asInstanceOf[Iterable[A]], f)
		  case _ => f(x, y)
		}
	  }
      val ita = xs.elements.toList
      val itb = that.elements.toList
      var res = true
      (ita, itb) match {
        case (Nil, Nil) => true
        case (a: anyIterable, b: anyIterable) => {
          if (a.firstOption.isDefined && b.firstOption.isDefined) {
            val (x, y, resta, restb) = (a.head, b.head, a.drop(1), b.drop(1))
            matchTwo(x, y) && resta.sameElementsAs(restb, f) ||
            resta.exists(matchTwo(_, y)) && restb.exists(matchTwo(_, x)) && 
              resta.removeFirst(matchTwo(_, y)).sameElementsAs(restb.removeFirst(matchTwo(_, x)), f)
          }
          else
            false
        }
        case _ => ita == itb  
      } 
    }

    /**
     * adds the sameElementsAs method to any object in order to do that comparison recursively 
     */
    implicit def anyToSameElements(x: Any) = new AnyWithSameElements(x)

    /**
     * Class adding the <code>sameElementsAs</code> method to any object. The default implementation uses standard equality (==) 
     */
    class AnyWithSameElements(x: Any) { 
       def sameElementsAs(that: Any): Boolean = x == that 
    }
  }
}
