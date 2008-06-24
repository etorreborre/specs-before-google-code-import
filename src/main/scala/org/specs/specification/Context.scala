package org.specs
import org.specs.util._
import org.specs.util.ExtendedString._
import scala.xml._
import org.specs.matcher._
import scala.collection.mutable._
import org.specs.runner._
import org.specs.matcher.MatcherUtils._
import org.specs.SpecUtils._
import org.specs.specification._
import org.specs.ExtendedThrowable._

/**
 * This traits adds before / after capabilities to specifications, so that a context can be defined for
 * each system under test being specified.
 */
trait Contexts extends SpecificationStructure {
  /** 
   * @deprecated
   * adds a "before" function to the last sut being defined 
   */
  def usingBefore(actions: () => Any) = currentSut.before = Some(actions)

  /** adds a "before" function to the last sut being defined */
  def doBefore(actions: =>Any) = currentSut.before = Some(() => actions)

  /** 
   * @deprecated
   * adds an "after" function to the last sut being defined 
   */
  def usingAfter(actions: () => Any) = currentSut.after = Some(actions)

  /** 
   * adds an "after" function to the last sut being defined 
   */
  def doAfter(actions: =>Any) = currentSut.after = Some(() => actions)

  /** 
   * repeats examples according to a predicate 
   */
  def until(predicate: =>Boolean) = currentSut.untilPredicate = Some(() => predicate)

  /** 
   * Syntactic sugar for before/after actions.<p>
   * Usage: <code>"a system" should { createObjects.before
   *  ...
   * </code>
   */
  implicit def toSutActions(actions: =>Unit) = SutActions(() => actions)

  /** 
   * Syntactic sugar for before/after actions.<p>
   * Usage: <code>"a system" should { createObjects.before
   *  ...
   * </code>
   */
  case class SutActions(actions: () =>Unit) {
    def before = usingBefore(actions)
    def after = usingAfter(actions)
  }

  /** 
   * Syntactic sugar to create pass a new context before creating a sut.<p>
   * Usage: <code>"a system" ->(context) should { 
   *  ...
   * </code>
   * In that case before/after actions defined in the context will be set on the defined sut.
   */
  implicit def toContext(s: String) = ToContext(s) 

  /** 
   * Syntactic sugar to create pass a new context before creating a sut.<p>
   * Usage: <code>"a system" ->(context) should { 
   *  ...
   * </code>
   * In that case before/after actions defined in the context will be set on the defined sut.
   */
  case class ToContext(s: String) {
    def ->-(context: Context): Sut = {
      if (context == null) throw new NullPointerException("the context is null")
      val sut = specify(s)
      usingBefore(context.beforeActions)
      usingAfter(context.afterActions)
      until(context.predicate())
      sut
    } 
  }

  /** Factory method to create a context with before only actions */
  def beforeContext(actions: => Any) = new Context { before(actions) }

  /** Factory method to create a context with before only actions and an until predicate */
  def beforeContext(actions: => Any, predicate: =>Boolean) = new Context { before(actions); until(predicate()) }

  /** Factory method to create a context with after only actions */
  def afterContext(actions: => Any) = new Context { after(actions) }

  /** Factory method to create a context with after only actions and an until predicate */
  def afterContext(actions: => Any, predicate: =>Boolean) = new Context { after(actions); until(predicate()) }

  /** Factory method to create a context with after only actions */
  def context(b: => Any, a: =>Any) = new Context { before(b); after(a) }

  /** Factory method to create a context with after only actions and an until predicate */
  def context(b: => Any, a: =>Any, predicate: =>Boolean) = new Context { before(b); after(a); until(predicate()) }
}
/** 
 * Case class holding before and after functions to be set on a system under test.<p>
 * Context objects are usually created using the factory methods of the Contexts trait:<pre>
 * 
 * // this method returns a context object which can be passed to a System under test (with "a system" ->(context) should {... )
 * // so that initSystem is done before each example and so that each example is repeated until enoughTestsAreExecuted is true 
 * beforeContext(initSystem).until(enoughTestsAreExecuted)
 * </pre>
 */
case class Context {
  var beforeActions: () => Any = () => () 
  var afterActions: () => Any = () => ()
  var predicate: () => Boolean = () => true
  def before(actions: =>Any) = { beforeActions = () => actions; this }
  def after(actions: =>Any) = { afterActions = () => actions; this }
  def until(predicate: =>Boolean) = { this.predicate = () => predicate; this }
}
