/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.specification
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
import scala.reflect.Manifest
import org.specs.execute._

/**
 * This traits adds before / after capabilities to specifications, so that a context can be defined for
 * each system under test being specified and trigger some actions before or after each example.
 */
trait BeforeAfter { outer: BaseSpecification =>
  /** adds a "before" function to the last sus being defined */
  def doBefore(actions: =>Any) = usingBefore(() => actions)
  /** adds a "around" function to the last sus being defined */
  def doAroundExpectations(actions: Any =>Any) = current.map(_.aroundExpectations = Some(actions))
  /**  adds an "after" function to the last sus being defined */
  def doAfter(actions: =>Any) = usingAfter(() => actions)
  /** adds a "firstActions" function to the last sus being defined */
  def doFirst(actions: =>Any) = current.map(stackFirstActions(_, actions))
  /** adds a "lastActions" function to the last sus being defined */
  def doLast(actions: =>Any) = current.map(stackLastActions(_, actions)) 
  /** adds a "beforeSpec" function to the current specification */
  def doBeforeSpec(actions: =>Any) = beforeSpec = stackActions(() => actions, beforeSpec)
  /** adds a "afterSpec" function to the current specification */
  def doAfterSpec(actions: =>Any) = afterSpec = reverseStackActions(() => actions, afterSpec)
  /** adds a "before" function to the last sus being defined */
  private def usingBefore(actions: () => Any) = current.map(stackBeforeActions(_, actions))
  /** adds an "after" function to the last sus being defined */
  private def usingAfter(actions: () => Any) = current.map(stackAfterActions(_, actions))
  /** actions are stacked so that several before actions can be triggered one after the other */
  private[specs] def stackBeforeActions(sus: Examples, actions: () => Any) = sus.before = stackActions(actions, sus.before)
  /** actions are stacked so that several before actions can be triggered one after the other */
  private[specs] def stackAroundActions(sus: Examples, actions: Any => Any) = sus.aroundExpectations = stackAround(actions, sus.aroundExpectations)
  /** adds an "after" function to a sus */
  private[specs] def stackAfterActions(sus: Examples, actions: () => Any) = sus.after = reverseStackActions(actions, sus.after)
  /** adds "firstActions" to a sus */
  private[specs] def stackFirstActions(sus: Examples, actions: =>Any) = sus.firstActions = stackActions(() => actions, sus.firstActions)
  /** adds "lastActions" to a sus */
  private[specs] def stackLastActions(sus: Examples, actions: =>Any) = sus.lastActions = reverseStackActions(() => actions, sus.lastActions)
  /** repeats examples according to a predicate */
  def until(predicate: =>Boolean): Unit = current.map(until(_, predicate))
  /** repeats examples according to a predicate */
  def until(sus: Examples, predicate: =>Boolean) = sus.untilPredicate = Some(() => { predicate })
  /** @return a function with actions being executed after the previous actions. */
  private def stackActions(actions: () => Any, previousActions: Option[() => Any]) = {
    Some(() => {
      previousActions.map(a => a())
      actions()
    })
  }
  /** @return a function with actions being stacked around. */
  private def stackAround(actions: Any => Any, previousActions: Option[Any => Any]) = {
    Some((a:Any) => {
      actions(previousActions.map(f => f(a)))
    })
  }
  /** @return a function with actions being executed before the previous actions. */
  private def reverseStackActions(actions: () => Any, previousActions: Option[() => Any]) = {
    Some(() => {
      actions()
      previousActions.map(a => a())
    })
  }
  /** 
   * Syntactic sugar for before/after actions.<p>
   * Usage: <code>"a system" should { createObjects.before
   *  ...
   * </code>
   */
  implicit def toShortActions(actions: =>Unit) = new ShortActions(actions)
  /** 
   * Syntactic sugar for before/after actions.<p>
   * Usage: <code>"a system" should { createObjects.before
   *  ...
   * </code>
   */
  class ShortActions(actions: =>Unit) {
    def before = doBefore(actions)
    def after = doAfter(actions)
    def doFirst: Unit = outer.doFirst(actions)
    def doLast: Unit = outer.doLast(actions)
    def beforeSpec = outer.doBeforeSpec(actions)
    def afterSpec = outer.doAfterSpec(actions)
  }
  /** 
   * Syntactic sugar for actions around expectations.<p>
   * Usage: <code>"a system" should { (a => createObjects(a)).aroundExpectations
   *  ...
   * </code>
   */
  implicit def toShortActions2(actions: Any => Any) = new ShortActions2(actions)
  /** 
   * Syntactic sugar for actions around expectations.<p>
   * Usage: <code>"a system" should { (a => createObjects(a)).aroundExpectations
   *  ...
   * </code>
   */
  class ShortActions2(actions: Any => Any) {
    def aroundExpectations = doAroundExpectations(actions)
  }
}
/**
 * This trait helps creating Context objects encapsulating the before/after actions which can be associated to a sus
 */
trait Contexts extends BeforeAfter { this: BaseSpecification =>
  /** Factory method to create a context with beforeAll only actions */
  def contextFirst(actions: => Any) = new Context { first(actions) }
  /** Factory method to create a context with before only actions */
  def beforeContext(actions: => Any) = new Context { before(actions) }
  /** Factory method to create a context with around only actions */
  def aroundExpectationsContext(actions: Any => Any) = new Context { aroundExpectations(actions) }
  /** Factory method to create a context with before only actions and an until predicate */
  def beforeContext(actions: => Any, predicate: =>Boolean) = new Context { before(actions); until(predicate()) }
  /** Factory method to create a context with after only actions */
  def afterContext(actions: => Any) = new Context { after(actions) }
  /** Factory method to create a context with afterAll actions */
  def contextLast(actions: => Any) = new Context { last(actions) }
  /** Factory method to create a context with after only actions and an until predicate */
  def afterContext(actions: => Any, predicate: =>Boolean) = new Context { after(actions); until(predicate()) }
  /** Factory method to create a context with after only actions */
  def context(b: => Any, a: =>Any) = new Context { before(b); after(a) }
  /** Factory method to create a context with before/after actions */
  def globalContext(b: => Any, a: =>Any) = new Context { first(b); last(a) }
  /** Factory method to create a context with before/after actions and an until predicate */
  def context(b: => Any, a: =>Any, predicate: =>Boolean) = new Context { before(b); after(a); until(predicate()) }
  /** 
   * Syntactic sugar to create pass a new context before creating a sus.<p>
   * Usage: <code>"a system" ->(context) should { 
   *  ..
   * </code>
   * In that case before/after actions defined in the context will be set on the defined sus.
   */
  implicit def whenInContext(s: String) = ToContext(s) 
  /** 
   * Syntactic sugar to create pass a new context before creating a sus.<p>
   * Usage: <code>"a system" ->(context) should { 
   *  ...
   * </code>
   * In that case before/after actions defined in the context will be set on the defined sus.
   */
  case class ToContext(desc: String) {
    def ->-[S](context: Context): Sus = {
      if (context == null) throw new NullPointerException("the context is null")
      specifySus(context, desc)
    } 
  }
  private def specifySus(context: Context, desc: String): Sus = {
    if (context == null) throw new NullPointerException("the context is null")
    val sus = specify(desc)
    stackFirstActions(sus, context.firstActions())
    stackBeforeActions(sus, context.beforeActions)
    stackAroundActions(sus, context.aroundExpectationsActions)
    stackAfterActions(sus, context.afterActions)
    stackLastActions(sus, context.lastActions())
    until(sus, context.predicate())
    sus
  }
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
case class Context() {
  private[specs] var firstActions: () => Any = () => () 
  private[specs] var lastActions: () => Any = () => ()
  private[specs] var beforeActions: () => Any = () => () 
  private[specs] var aroundExpectationsActions: Any => Any = (a:Any) => () 
  private[specs] var afterActions: () => Any = () => ()
  private[specs] var predicate: () => Boolean = () => true
  def before(actions: =>Any) = { beforeActions = () => actions; this }
  def aroundExpectations(actions: Any =>Any) = { aroundExpectationsActions = actions; this }
  def after(actions: =>Any) = { afterActions = () => actions; this }
  def first(actions: =>Any) = { firstActions = () => actions; this }
  def last(actions: =>Any) = { lastActions = () => actions; this }
  def until(predicate: =>Boolean) = { this.predicate = () => predicate; this }
}


