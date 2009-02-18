package org.specs.matcher
import org.scalacheck.{Gen, Prop, Arg, Test}
import org.scalacheck.util.StdRand
import org.scalacheck.Prop._
import org.scalacheck.Test.{Status, Params, Proved, Passed, Failed, Exhausted, GenException, PropException, Result}
import org.scalacheck.Pretty._
import org.scalacheck.ConsoleReporter._
import scala.collection.immutable.HashMap
import org.specs.io.ConsoleOutput
import org.specs.matcher._
import org.specs.matcher.MatcherUtils.q
import org.specs.specification.FailureException
import org.specs.specification._
/**
 * The <code>ScalaCheckMatchers</code> trait provides matchers which allow to
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">ScalaCheck project</a>
 */
trait ScalaCheckMatchers extends ConsoleOutput with ScalaCheckFunctions with ScalaCheckParameters with SuccessValues with ExpectationsListener {

  /**
   * This implicit value is useful to transform the SuccessValue returned by matchers to properties.
   * More specifically, this allows to write expectations as properties:
   * (1 + 1) must_== 2 will return a SuccessValue if it is not throwing a FailureException.
   * That success value will can then be considered as a property in an example:
   *
   * <code>{ (1 + 1) must_== 2 } must pass</code>
   *
   * @see Expectable
   */
  implicit val successValueToProp: SuccessValue => Prop = (s: SuccessValue) => Prop.forAll((a: Boolean) => true)

   /**
    * default parameters. Uses ScalaCheck default values and doesn't print anything to the console
    */
   implicit def defaultParameters = new Parameters(setParams(Nil))

   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>function must pass(generated_values)</code><br>
    * @param params are the given by the implicit default parameters of ScalaCheck
    */
   def pass[T](g: Gen[T])(implicit params: Parameters) = new Matcher[T => Boolean]() {
      def apply(f: => (T => Boolean)) = checkFunction(g)(f)(params)
    }

   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>generated_values must pass(function)</code>
    */
   def pass[T, S](f: T => Boolean)(implicit params: Parameters) = new Matcher[Gen[T]](){
      def apply(g: => Gen[T]) = checkFunction(g)(f)(params)
   }

   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>generated_values must pass(property)</code>
    */
   def pass[T](prop: Prop)(implicit params: Parameters) = new Matcher[Gen[T]](){
     def apply(g: => Gen[T]) = checkProperty(forAllProp(g)(a => prop))(params)
   }

   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>property must pass</code>
    */
    def pass(implicit params: Parameters) = new Matcher[Prop](){
     def apply(p: => Prop) = checkProperty(p)(params)
    }

   def checkFunction[T](g: Gen[T])(f: T => Boolean)(p: Parameters) = {
      // create a scalacheck property which states that the function must return true
      // for each generated value
      val prop = forAllProp(g)(a => if (f(a)) proved else falsified)
      checkProperty(prop)(p)
   }
   /**
    * checks if the property is true for each generated value, and with the specified
    * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
    * and indicates if the generation should be verbose or not
    */
   def checkProperty(prop: Prop)(p: Parameters) = {
     checkScalaCheckProperty(prop)(Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand, 1, 1), p.verbose)
   }

  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  def checkScalaCheckProperty(prop: Prop)(params: Params, verbose: Boolean) = {
     // will print the result of each test if verbose = true
     def printResult(succeeded: Int, discarded: Int): Unit = {
       if (!verbose) return
       if (discarded == 0)
         printf("\rPassed %d tests", succeeded)
       else
         printf("\rPassed %d tests; %d discarded", succeeded, discarded)
       flush
     }

     // check the property
     def propToCheck = if (!shouldCountExpectations) prop else (prop && Prop.forAll((t: Boolean) => true.isExpectation))
     val results = checkProp(params, propToCheck, printResult)

     // display the final result if verbose = true
     if (verbose) {
       val s = prettyTestRes.pretty(results)
       printf("\r%s %s%s\n", if (results.passed) "+" else "!", s, List.make(70 - s.length, " ").mkString(""))
     }

     results match {
       case Result(Proved(as), succeeded, discarded, _) => (true,  noCounterExample(succeeded), "A counter-example was found " + afterNTries(succeeded))
       case Result(Passed, succeeded, discarded, _) => (true,  noCounterExample(succeeded), "A counter-example was found " + afterNTries(succeeded))
       case r@Result(GenException(e), n, _, _) => (false, noCounterExample(n), prettyTestRes.pretty(r))
       case r@Result(Exhausted, n, _, _)     => (false, noCounterExample(n), prettyTestRes.pretty(r))
       case Result(Failed(args, _), n, _, _) =>
         (false, noCounterExample(n), "A counter-example is "+counterExample(args)+" (" + afterNTries(n) + afterNShrinks(args) + ")")
       case Result(PropException(args, FailureException(ex), _), n, _, _) =>
         (false, noCounterExample(n), "A counter-example is "+counterExample(args)+": " + ex + " ("+afterNTries(n)+")")
       case r@Result(PropException(m, ex, _), n, _, _) =>
         (false, noCounterExample(n), prettyTestRes.pretty(r))
     }
   }
   // depending on the result, return the appropriate success status and messages
   // the failure message indicates a counter-example to the property
   protected [matcher] def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)
   protected [matcher] def afterNTries(n: Int) = "after " + (if (n == 1) n + " try" else n + " tries")
   protected [matcher] def afterNShrinks(args: List[Arg]) = {
     if (args.forall(_.shrinks == 0))
       ""
     else
       args.map { arg =>
         if (arg.origArg != arg.arg)
           q(arg.origArg) +" -> " + q(arg.arg)
         else
           " = "
      }.mkString(" - shrinked (", ",", ")")
   }

   protected [matcher] def counterExample(args: List[Arg]) = {
     if (args.size == 1)
       args.map(a => if (a.arg == null) "null" else a.arg.toString).mkString("'", "", "'")
     else if (args.exists(_.arg.toString.isEmpty))
       args.map(_.arg).mkString("['", "', '", "']")
     else
       args.map(_.arg).mkString("[", ", ", "]")
   }

}
/**
 * This trait is used to facilitate testing by mocking ScalaCheck functionalities
 */
trait ScalaCheckFunctions {
  def checkProp(params: Params, prop: Prop, printResult: (Int, Int) => Unit) = Test.check(params, prop, printResult)
  def forAllProp[A,P](g: Gen[A])(f: A => Prop): Prop = Prop.forAll(g)(f)
}
/**
 * This trait provides generation parameters to use with the <code>ScalaCheckMatchers</code>
 */
trait ScalaCheckParameters {
  /**
   * Values which can be used as Symbol aliases to specify ScalaCheck parameters<br>
   * The naming is a bit different, in order to keep short names for frequent use cases<ul>
   *  <code><li>minTestsOk == minSuccessfulTests
   *  <li>maxDiscarded == maxDiscardedTests
   *  <li>minSize and maxSize keep their name <code><ul>
   */
  val (minSize, maxSize, maxDiscarded, minTestsOk) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk)

  /** This variable is used to track if we need to add an expectation each time a property is evaluated */
  private var countExpectations = true
  /** declare that an expectation should be added each time a property is evaluated (default) */
  def expectProperties() = { countExpectations = true; this }
  /** declare that no expectation should be added each time a property is evaluated */
  def dontExpectProperties() = { countExpectations = false; this }
  def shouldCountExpectations = countExpectations
  /**
   * Default values for ScalaCheck parameters
	 */
  def defaultValues = Map(minTestsOk->100, maxDiscarded ->500, minSize->0, maxSize->100)

  /**
   * This object is used to set parameters but nothing will be printed to the console<br>
   * Usage: <pre><code>
   * generated_values must pass { v =>
   *   property(v) mustBe ok
   * }(set(minTestsOk->15, maxDiscarded->20))</code></pre>
   */
  object set extends Parameters(setParams(Nil)) {
    def apply(p: (Symbol, Int)*) = new Parameters(setParams(p))
  }

  /**
   * Those parameters will print the result on the console and use the default settings, or specified parameters <br>
   * Usage: <pre><code>
   * generated_values must pass { v =
   *   property(v) mustBe ok
   * }(display) </code></pre>
   *
   *  or
   *
   *  generated_values must pass { v =>
   *    property(v) mustBe ok
   *  }(display(minTestsOk->15, maxDiscarded->20))</code></pre>
   */
  object display  extends Parameters(setParams(Nil)) {
    def apply(p: (Symbol, Int)*) = new Parameters(setParams(p)) { override def verbose = true }
    override def verbose = true
  }

  /**
   * This function transform the varargs parameters into a Map with default values
   * if some expected values are not provided by the user
   */
  def setParams(p: Seq[(Symbol, Int)]): Map[Symbol, Int] = {
    var params: Map[Symbol, Int] = new HashMap[Symbol, Int]
    p foreach { pair: (Symbol, Int) =>
        //  this is a useful check in case of print(null) or set(null)
        if (pair == null || pair._1 == null)
          throw new RuntimeException("null values are not accepted in scalacheck parameters: " + q(pair))
        else {
          val (s, i) = pair
          params = params + Pair(s, i)
        }
    }
    params.withDefault(defaultValues)
  }
}
/**
 * This class is the base class for the display and set case classes.<br>
 * It contains a Map of generation parameters and indicates if the generation
 * must be verbose.
 */
case class Parameters(params: Map[Symbol, Int]) {
  def apply(s: Symbol) = params(s)
  def verbose = false
}
