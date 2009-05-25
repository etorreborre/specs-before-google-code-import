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
 * DEALINGS INTHE SOFTWARE.
 */
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
  def executeExample(ex: Example): this.type = { ex.execute; this }
  def executeTest(ex: Example, t: =>Any): Any = ex.execute(t)
  def afterExample(ex: Example) = {
    example = None
  }
}
/** Default LifeCycle with no actions before or after. */
object DefaultLifeCycle extends ExampleLifeCycle
