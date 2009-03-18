package org.specs.mock
import org.specs.specification._
import org.specs.runner._

object numberOfInvocations extends numberOfInvocations
class numberOfInvocations extends LiterateSpecification("Number of invocations") with MockitoSpecification {
<wiki>
{"""import org.specs.Specification
  import org.specs.mock.Mockito
  import org.mockito.Mock
  import java.util.List
  import java.util.LinkedList""" prelude it shh }
h3. Number of invocations

The number of invocations can be checked with different methods on the @called@ matcher: {"""

  class s4 extends Specification with Mockito {
    val mockedList = mock[List[String]]

    mockedList.add("one")

    2.times { i => mockedList.add("two") } 
    3.times { i => mockedList.add("three") } 
  } 
""" prelude it }

<ex>@was called.once@ is the same as @was called@</ex>:

{ "new s4 { mockedList.add(\"one\") was called.once }.isOk" snip it }
{ >("true") }

<ex>@was called.twice@ checks if the method was called twice</ex>:

{ "new s4 { mockedList.add(\"two\") was called.twice }.isOk" snip it }
{ >("true") }

<ex>It is also possible to check that a method was called at least a number of times</ex>:

{ "new s4 { mockedList.add(\"two\") was called.atLeastOnce }.isOk" snip it }
{ >("true") }

<ex>If the method wasn't called the expected number of times, there must be a @FailureException@</ex>:
  
{ "new s4 { mockedList.add(\"one\") was called.twice }.failures" snip it }
{ >("The method was not called as expected: list.add(\"one\"); Wanted 2 times but was 1") }

<ex>@wasnt called@ checks that the method wasn't called at all (never in Mockito)</ex>:
  
{ "new s4 { mockedList.add(\"one\") wasnt called }.failures" snip it }
{ >("The method was not called as expected: list.add(\"one\"); Never wanted but invoked!") }

<ex>It is also possible to check that there are no unexpected calls on a mock</ex>:
  
{ """  new s4 { 
    mockedList.add("one") was called
    mockedList had noMoreCalls
  }.failures.first""" snip it }
{ >("The mock was called: No interactions wanted") }

</wiki> isSus
}
