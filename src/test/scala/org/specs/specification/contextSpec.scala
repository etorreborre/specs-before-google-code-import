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
import org.specs._
import org.specs.runner._

class contextSpec extends HtmlSpecificationWithJUnit("Contexts specification") with ContextDefinitions {
  override def htmlDir = "."

  "Contexts" ->> <wiki>

There are 2 types of contexts that can be set on a System under specification, to provide a way to manage the data that examples are using:
* shared contexts: {linkTo("Shared contexts")}
* system contexts: {linkTo("System contexts")}

Shared contexts allow to defined @before@ and @after@ operations that will set up and clear the specification variables used by the examples so that each example will operate on a known state.

On the other hand system contexts provide a way to specify a @SystemContext@ object which will be passed to each example for its sole usage. This way of using contexts is a bit more verbose than defining shared contexts but it has 2 advantages over shared contexts:
* the set of data the example is operating on is clearly defined as a "System"
* examples can be executed in parallel because they don't share the same variables

</wiki>

"Shared contexts".definedAs(shared) ->> <wiki>

h4. Introduction

Shared contexts are defined using the before/after methods on the Context class, to set or reset those variables before and after execution of the examples. They usually share variables defined on the specification object.

h4. Examples

<ex>A sus with a shared context should use its before method to set shared variables before executing an example</ex>{
  exampleOk(0) }
<ex>If two examples refer to the same variable, modifications made by the first example can be seen by the second one</ex>{
  exampleOk(1) }
</wiki>

"System contexts".definedAs(systemContexts) ->> <wiki>

System contexts are defined by subclassing the @SystemContext@ class and by defining the @newISystem@ method which will provide a fresh instance of the system, in a specific context.
The construction of the system and its initialization can be separated by doing the initialization of the system in the @newInstance@ method while overriding the @before@ method to set up specific variables on the system. This helps in defining a hierarchy of contexts differing only by their @before@ methods.

h4. Examples

<ex>A sus with a system context should pass a system instance to the examples.</ex>{ exampleOk(0) }
<ex>Each example should get a fresh copy of the system in its specific context.</ex>{ exampleOk(1) }

h4. Parameters

<ex>Examples can either be given the system under specification or the system and its context</ex>: {
"""use the system as a parameter in { s: System =>
  ...
}
use the system and its as parameters in { (s: System, c: Context) =>
  ...
}
""" >@}{ parametersOk }
</wiki>

"System contexts with shared examples".definedAs(sharedExamples) ->> <wiki>

<ex>When examples are shared between SUS, each example should be executed with the SUS context it is executed in</ex>: {
"""
    "the first sus".definedAs(context1) should {
      "use context 1 in its examples" in { (system: System, context: NamedContext) =>
        context.name must_== expectedCurrentContextName
      }
      doAfter { expectedCurrentContextName = "context2"}
    }
    "this sus".definedAs(context2) should {
      behave like "the first sus" // but with context2 instead of context1
    }
""" >@
}{ sharedExamplesMustHaveProperContexts }

</wiki>



  def parametersOk = exampleOk(2)
  def exampleOk(i: Int) = eg { (s: Specification) =>
    executeSpec(s)
    exampleIsOk(s.examples(i))
  }
  def exampleIsOk(e: Example) = e.isOk aka e.description.toString must beTrue
  def executeSpec(s: Specification) = s.failures // execute all examples
  def sharedExamplesMustHaveProperContexts = eg { (s: SpecificationWithSystemContextAndSharedExamples) =>
    executeSpec(s)
    executedContexts(0) must_== "context2"
    executedContexts(1) must_== "context1"
  }
}
trait ContextDefinitions extends SystemContexts {
  case class SpecificationWithSharedContext() extends Specification {
    var sharedCounter = 0
    val sharedContext = beforeContext { sharedCounter = sharedCounter + 1 }
    "A sus with a" ->- sharedContext should {
      "increment a shared variable before the first example" in { sharedCounter must_== 1 }
      "increment a shared variable before the second example" in { sharedCounter must_== 2 }
    }
  }
  def shared = new SystemContext[SpecificationWithSharedContext] {
    def newSystem = SpecificationWithSharedContext()
  }
  case class SpecificationWithSystemContext() extends Specification with SystemContexts {
    var system1: System = _
    case class System() {
      var counter = 0
    }
    class SampleSystemContext extends SystemContext[System] {
      var count = 0
      def newSystem = new System()
      override def before(s: System) = {
        s.counter = s.counter + 1
        count = 0
      }
    }
    def initializedWithASystem = new SampleSystemContext
    "In those example, the system is".definedAs(initializedWithASystem) should {
      "the system has be passed to the example and initialized" in { system: System =>
        system1 = system
        system.counter must_== 1
      }
      "the passed system was a fresh copy" in { system: System =>
        system1 mustNotEq system
        system.counter must_== 1
      }
      "the system and its context can be passed" in { (system: System, context: SampleSystemContext) =>
        system must haveClass[System]
        context must haveClass[SampleSystemContext]
        context.count must_== 0
      }
    }
  }
  def systemContexts = new SystemContext[SpecificationWithSystemContext] {
    def newSystem = SpecificationWithSystemContext()
  }

  var executedContexts: List [String] = Nil
  case class SpecificationWithSystemContextAndSharedExamples() extends Specification with SystemContexts {
    case class System()
    case class NamedContext(name: String) extends SystemContext[System] {
      def newSystem = System()
    }
    def context1 = NamedContext("context1")
    def context2 = NamedContext("context2")

    "the first sus".definedAs(context1) should {
      "use context 1 in its examples" in { (system: System, context: NamedContext) =>
        executedContexts = context.name :: executedContexts
        1 must_== 1
      }
    }
    "the second sus".definedAs(context2) should {
      behave like "the first sus"
    }
  }
  def sharedExamples = new SystemContext[SpecificationWithSystemContextAndSharedExamples] {
    def newSystem = SpecificationWithSystemContextAndSharedExamples()
  }
}
