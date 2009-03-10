package org.specs.specification

/**
 * This abstract trait is used to represent how examples should be executed:<ul>
 * <li>sequentially or not ("not" is the default)
 * <li>with functions being executed before / after the example
 * <li>with functions being executed before / after the example tests
 * </ul>
 */
trait ExampleLifeCycle {
  protected var sequential = false
  def isSequential = sequential
  def setSequential() = sequential = true

  protected[this] var example: Option[Example] = None
  def until = true
  def beforeExample(ex: Example) = { example = Some(ex) }
  def beforeTest(ex: Example)= {}
  def afterTest(ex: Example) = {}
  def executeTest(ex: Example, t: =>Any): Any = ex.execute(t)
  def afterExample(ex: Example) = {
    example = None
  }
}
/** Default LifeCycle with no actions before or after. */
object DefaultLifeCycle extends ExampleLifeCycle
