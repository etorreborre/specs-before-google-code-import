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
  import org.mockito.Mock
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
  } """ add it }{ executeIsNot("error") }

h4. Failures

If one method has not been called on a mock, <ex>the @was called@ matcher must throw a @FailureException@</ex>: {"""

  object s2 extends Specification with Mockito {
    val m = mock[List[String]]
    m.clear() was called
  }
  s2.failures
  """ snip it }
  { >("The method was not called as expected: Wanted but not invoked: list.clear()") }

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

<ex>Calling a stubbed method with @returns@ returns the expected value</ex>. For example, the following prints "first":

{ "s3.mockedList.get(0)" snip it }
{ >("first") }

<ex>Calling a stubbed method with @throws@ throws the expected exception</ex>. For example, the following throws a RuntimeException:

{ "s3.mockedList.clear()" snip it }
{ >("RuntimeException") }

<ex>Calling a non-stubbed method should return a default value</ex>. For example, the following returns @null@ because @get(999)@ was not stubbed:
  
{ "s3.mockedList.get(999)" snip it }
{ >("null") }

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
{ >("true") }

<ex>@was called.twice@ checks if the method was called twice</ex>:

{ """new s4 { mockedList.add("two") was called.twice }.isOk""" snip it }
{ >("true") }

<ex>It is also possible to check that a method was called at least a number of times</ex>:

{ """new s4 { mockedList.add("two") was called.atLeastOnce }.isOk""" snip it }
{ >("true") }

<ex>If the method wasn't called the expected number of times, there must be a @FailureException@</ex>:
  
{ """new s4 { mockedList.add("one") was called.twice }.failures""" snip it }
{ >("The method was not called as expected: list.add(\"one\"); Wanted 2 times but was 1") }

<ex>@wasnt called@ checks that the method wasn't called at all (never in Mockito)</ex>:
  
{ """new s4 { mockedList.add("one") wasnt called }.failures""" snip it }
{ >("The method was not called as expected: list.add(\"one\"); Never wanted but invoked!") }

<ex>It is also possible to check that there are no unexpected calls on a mock</ex>:
  
{ """  new s4 { 
    mockedList.add("one") was called
    mockedList had noMoreCalls
  }.failures.first""" snip it }
{ >("The mock was called: No interactions wanted") }

h3. Annotations

<ex>It is possible to use annotations to declare mocks</ex> {"""

  object s5 extends Specification with Mockito {
    // do we gain anything using Scala, compared to val mockedList = mock[List[String]]?
    @Mock val mockedList: List[String] = null  
    "this needs to be inside an example because otherwise a NPE is thrown" in {
      mockedList.clear()
      mockedList.clear() was called
    }
  }
""" snip it }
{ "s5.isOk" add it }
{ >("true") } 

h3. Stubbing consecutive calls (iterator-style stubbing)

Sometimes we need to stub with different return value/exception for the same method call. Typical use case could be mocking iterators. Original version of Mockito did not have this feature to promote simple mocking. For example, instead of iterators one could use Iterable or simply collections. Those offer natural ways of stubbing (e.g. using real collections). 
In rare scenarios stubbing consecutive calls could be useful, though: {"""

  object s6 extends Specification with Mockito {
    val mockedList = mock[List[String]]

    mockedList.get(0) returns "hello" thenReturns "world"
  }
""" snip it }

<ex>The first call returns the first value</ex>:

{ "s6.mockedList.get(0)" add it }
{ >("hello") }

<ex>The second call returns the second value</ex>:

{ "s6.mockedList.get(0)" add it }
{ >("world") }

When several values need to be stubbed this version of returns would also work: {"""

  object s7 extends Specification with Mockito {
    val mockedList = mock[List[String]]
    mockedList.get(0) returns ("hello", "world")
  }
""" snip it }

<ex>The first value is "hello"</ex>:
{ "s7.mockedList.get(0)" add it }
{ >("hello") }

<ex>The second value is "world"</ex>:
{ "s7.mockedList.get(0)" add it }
{ >("hello") }

when(mock.someMethod("some arg"))
   .thenThrow(new RuntimeException())
   .thenReturn("foo");
 
 //First call: throws runtime exception:
 mock.someMethod("some arg");
 
 //Second call: prints "foo"
 System.out.println(mock.someMethod("some arg"));
 
 //Any consecutive call: prints "foo" as well (last stubbing wins). 
 System.out.println(mock.someMethod("some arg"));
 
Alternative, shorter version of consecutive stubbing:
 when(mock.someMethod("some arg"))
   .thenReturn("one", "two", "three");

</wiki> isSus

  include(argumentMatchers)
}
trait MockitoSpecification extends Mockito with Expectations with SnipIt with Wiki with Html with JUnit {
  def executeIs(s: String) = { execute(it) must include(s) }
  def >(s: String) = outputIs(s)
  def outputIs(s: String) = {
    val result = execute(it)
    var out = "> " + result
    try  { result must include(s) }
    catch { case e => out = "> " + e.getMessage }
    out
  }
  def executeIsNot(s: String) = execute(it) mustNot include(s)
}
