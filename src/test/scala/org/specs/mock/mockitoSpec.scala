package org.specs.mock
import org.specs._
import org.specs.specification._
import org.specs.runner._

class mockitoSpec extends LiterateSpecification("Mockito Specification") with MockitoSpecification {
  <wiki>
Mockito is a Java library for mocking.

  The following samples are taken from the main documentation which can be found "here":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Mockito.html

h3. Let's verify some behaviour

First of all, we need to import some classes and traits for our examples: {"""

  import org.specs.Specification
  import org.specs.mock.Mockito
  import java.util.List
  import java.util.LinkedList""" prelude it }

A mock is created with the @mock@ method: {"""

  object s extends Specification with Mockito {

    // mock creation
    val mockedList = mock[List[String]]

    // using mock object
    mockedList.add("one")
    mockedList.clear()""" snip it }

  // <ex>It is possible to check that some methods have been called on the mock with the @called@ matcher</ex>:

{"""    // verification
    mockedList.add("one") was called
    mockedList.clear() was called
  } """ addTo it }{ executeIsNot("error") }

h4. Failures

If one method has not been called on a mock, <ex>the @was called@ matcher must throw a FailureException</ex>: {"""

  object s extends Specification with Mockito {
    val m = mock[List[String]]
    m.clear() was called
  }
  s.failures.first.getMessage
  """ snip it }
  { outputIs("The method was not called: list.clear()") }

h4. Argument matchers

{ linkTo(argumentMatchers) } allow flexible verification or stubbing.

h3. How about some stubbing?

<ex>You can mock concrete classes, not only interfaces</ex> {"""

  object s extends Specification with Mockito {
    val mockedList = mock[LinkedList[String]]

    // stubbing
    mockedList.get(0) returns "first"
    mockedList.get(1) throws new RuntimeException
  }
""" prelude it }{ executeIsNot("error") }

<ex>Calling a stubbed method with @willReturn@ returns the expected value</ex>. For example, the following prints "first":

{ "s.mockedList.get(0)" snip it }
{ outputIs("first") }

<ex>Calling a stubbed method with @willThrow@ throws the expected exception</ex>. For example, the following throws a RuntimeException:

{ "s.mockedList.get(1)" snip it }
{ outputIs("RuntimeException") }

<ex>Calling a non-stubbed method should return a default value</ex>. For example, the following returns @null@ because @get(999)@ was not stubbed:
  
{ "s.mockedList.get(999)" snip it }
{ outputIs("null") }
  
  </wiki> isSus

  include(argumentMatchers)
}
trait MockitoSpecification extends Mockito with Expectations with SnipIt with Wiki with Html with JUnit {
  def executeIs(s: String) = { execute(it) must include(s) }
  def outputIs(s: String) = {
    val result = execute(it)
    var out = "> " + result
    try  { result must include(s) }
    catch { case e => out = "> " + e.getMessage }
    out
  }
  def executeIsNot(s: String) = execute(it) mustNot include(s)
}
