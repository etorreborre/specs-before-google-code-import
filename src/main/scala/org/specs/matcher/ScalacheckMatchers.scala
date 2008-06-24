package org.specs.matcher
import org.scalacheck.{StdRand, Gen, Prop, Arg, Test}
import org.scalacheck.Prop._
import org.scalacheck.Test.{Stats, Params, Proved, Passed, Failed, Exhausted, GenException, PropException, Result}
import org.scalacheck.ConsoleReporter._
import scala.collection.immutable.HashMap
import org.specs.io.ConsoleOutput
import org.specs.matcher._
import org.specs.matcher.MatcherUtils.q
import org.specs.specification.FailureException

/**
 * The <code>ScalacheckMatchers</code> trait provides matchers which allow to 
 * assess properties multiple times with generated data.
 * @see the <a href="http://code.google.com/p/scalacheck/">Scalacheck project</a>
 */
trait ScalacheckMatchers extends ConsoleOutput with ScalacheckFunctions with ScalacheckParameters {
   /**
    * default parameters. Uses Scalacheck default values and doesn't print to the console
    */
   implicit def defaultParameters = new Parameters(setParams(Nil))
		
   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>function must pass(generated_values)</code><br>
    * @param params are the given by the implicit default parameters of Scalacheck
    */
   def pass[T](g: Gen[T])(implicit params: Parameters) = new Matcher[T => Boolean](){
      def apply(f: => (T => Boolean)) = checkFunction(g)(f)(params)
    }

   /**
    * Matches ok if the <code>function T => Boolean</code> returns <code>true</code> for any generated value<br>
    * Usage: <code>generated_values must pass(function)</code>
    */
   def pass[T](f: T => Boolean)(implicit params: Parameters) = new Matcher[Gen[T]](){
      def apply(g: => Gen[T]) = checkFunction(g)(f)(params)
   }

   /**
    * Matches ok if the <code>property</code> is proved for any generated value<br>
    * Usage: <code>generated_values must pass(property)</code>
    */
   def pass[T](prop: Prop)(implicit params: Parameters) = new Matcher[Gen[T]](){
     def apply(g: => Gen[T]) = checkProperty(forAll(g)(a => prop))(params)
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
      val prop = forAll(g)(a => if (f(a)) proved else falsified)
      checkProperty(prop)(p)
   }
   /**
    * checks if the property is true for each generated value, and with the specified
    * generation parameters <code>p</code>. <code>p</code> is transformed into a scalacheck parameters
    * and indicates if the generation should be verbose or not 
    */
   def checkProperty(prop: Prop)(p: Parameters) = {
     checkScalacheckProperty(prop)(Params(p(minTestsOk), p(maxDiscarded), p(minSize), p(maxSize), StdRand), p.verbose)
   }
    
  /**
   * checks if the property is true for each generated value, and with the specified
   * scalacheck parameters. If verbose is true, then print the results on the console
   */
  def checkScalacheckProperty(prop: Prop)(params: Params, verbose: Boolean) = {
     // will print the result of each test if verbose = true
     def printResult(succeeded: Int, discarded: Int): Unit = {
       if (!verbose) return
       if (discarded == 0) 
         printf("\rPassed {0} tests", succeeded)
       else 
         printf("\rPassed {0} tests; {1} discarded", succeeded, discarded)
       flush
     }
     
     // check the property
     val statistics = check(params, prop, printResult) 
     
     // display the final result if verbose = true
     if (verbose) {
       val s = prettyTestStats(statistics)
       printf("\r{0} {1}{2}\n", if (statistics.result.passed) "+" else "!", s, List.make(70 - s.length, " ").mkString(""))
     }

     // depending on the result, return the appropriate success status and messages
     // the failure message indicates a counter-example to the property
     def afterNTries(n: Int) = "after " + (if (n <= 1) n + " try" else n + " tries")
     def noCounterExample(n: Int) = "The property passed without any counter-example " + afterNTries(n)
     def afterNShrinks(args: List[Arg]) = args.map(arg => if (arg.shrinks >= 1) arg.shrinks.toString else "").mkString(", ")
     def counterExample(args: List[Arg]) = args.map(_.arg).mkString(", ")
     statistics match {
       case Stats(Proved(as), succeeded, discarded) => (true,  noCounterExample(succeeded), "A counter-example was found " + afterNTries(succeeded)) 
       case Stats(Passed, succeeded, discarded) => (true,  noCounterExample(succeeded), "A counter-example was found " + afterNTries(succeeded)) 
       case s@Stats(GenException(e), n, _) => (false, noCounterExample(n), prettyTestStats(s)) 
       case s@Stats(Exhausted, n, _)     => (false, noCounterExample(n), prettyTestStats(s)) 
       case Stats(Failed(args), n, _) => 
         (false, noCounterExample(n), "A counter-example is '"+counterExample(args)+"' (" + afterNTries(n) + afterNShrinks(args) + ")") 
       case Stats(PropException(args, FailureException(ex)), n, _) => 
         (false, noCounterExample(n), "A counter-example is '"+counterExample(args)+"': " + ex + " ("+afterNTries(n)+")") 
       case s@Stats(PropException(m, ex), n, _) => 
         (false, noCounterExample(n), prettyTestStats(s)) 
     }
   }
  
}
/**
 * This trait is used to facilitate testing by mocking Scalacheck functionalities
 */
trait ScalacheckFunctions {
  def check(params: Params, prop: Prop, printResult: (Int, Int) => Unit) = Test.check(params, prop, printResult)
  def forAll[A,P](g: Gen[A])(f: A => Prop): Prop = Prop.forAll(g)(f)
}
/**
 * This trait provides generation parameters to use with the <code>ScalacheckMatchers</code>
 */
trait ScalacheckParameters {
  /**
   * Values which can be used as Symbol aliases to specify Scalacheck parameters<br>
   * The naming is a bit different, in order to keep short names for frequent use cases<ul>
   *  <code><li>minTestsOk == minSuccessfulTests
   *  <li>maxDiscarded == maxDiscardedTests
   *  <li>minSize and maxSize keep their name <code><ul>
   */
  val (minSize, maxSize, maxDiscarded, minTestsOk) = ('minSize, 'maxSize, 'maxDiscarded, 'minTestsOk)

  /**
   * Default values for Scalacheck parameters
	 */
  def defaultValues = Map(minTestsOk->100, maxDiscarded->500, minSize->0, maxSize->100) 

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
class Parameters(params: Map[Symbol, Int]) {
  def apply(s: Symbol) = params(s)
  def verbose = false
}
