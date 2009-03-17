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

  object s2 extends Specification with Mockito {
    val m = mock[List[String]]
    m.clear() was called
  }
  s2.failures
  """ snip it }
  { outputIs("The method was not called as expected: Wanted but not invoked: list.clear()") }

h4. Argument matchers

{ linkTo(argumentMatchers) } allow flexible verification or stubbing.

h3. How about some stubbing?

<ex>You can mock concrete classes, not only interfaces</ex> {"""

  object s3 extends Specification with Mockito {
    val mockedList = mock[LinkedList[String]]

    // stubbing
    mockedList.get(0) returns "first"
    mockedList.clear() throws new RuntimeException
  }
""" prelude it }{ executeIsNot("error") }

<ex>Calling a stubbed method with @willReturn@ returns the expected value</ex>. For example, the following prints "first":

{ "s3.mockedList.get(0)" snip it }
{ outputIs("first") }

<ex>Calling a stubbed method with @willThrow@ throws the expected exception</ex>. For example, the following throws a RuntimeException:

{ "s3.mockedList.clear()" snip it }
{ outputIs("RuntimeException") }

<ex>Calling a non-stubbed method should return a default value</ex>. For example, the following returns @null@ because @get(999)@ was not stubbed:
  
{ "s3.mockedList.get(999)" snip it }
{ outputIs("null") }

h3. Verifying the number of invocations

The number of invocations can be checked with different methods on the @called@ matcher: {"""

  class s4 extends Specification with Mockito {
    val mockedList = mock[List[String]]

    mockedList.add("one")

    2.times { i => mockedList.add("two") } 
    3.times { i => mockedList.add("three") } 
  } 
""" prelude it }

<ex>@was called.once@ is the same as @was called@</ex>:

{ """new s4 { mockedList.add("one") was called.once }.isOk""" snip it }
{ outputIs("true") }

<ex>@was called.twice@ checks if the method was called twice</ex>:

{ """new s4 { mockedList.add("two") was called.twice }.isOk""" snip it }
{ outputIs("true") }

<ex>It is also possible to check that a method was called at least a number of times</ex>:

{ """new s4 { mockedList.add("two") was called.atLeastOnce }.isOk""" snip it }
{ outputIs("true") }

<ex>If the method wasn't called the expected number of times, there must be a FailureException</ex>:
  
{ """new s4 { mockedList.add("one") was called.twice }.failures""" snip it }
{ outputIs("The method was not called as expected: list.add(\"one\"); Wanted 2 times but was 1") }

<ex>@wasnt called@ checks that the method wasn't called at all (never in Mockito)</ex>:
  
{ """new s4 { mockedList.add("one") wasnt called }.failures""" snip it }
{ outputIs("The method was not called as expected: list.add(\"one\"); Never wanted but invoked!") }


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
