
package org.specs.mock
import org.specs.specification._
import org.specs.runner._

object argumentMatchers extends argumentMatchers
class argumentMatchers extends LiterateSpecification("Argument Matchers") with MockitoSpecification {
<wiki>

h3. Argument matchers

Argument matchers allow flexible verification or stubbing. Click "here":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/Matchers.html for more details.

Let's import some definitions first: {"""  

  import org.specs._
  import org.specs.mock.Mockito
  import java.util.List
  import org.mockito.Matchers.{ argThat, anyInt, eq => isEq }
  import org.hamcrest.core.{ IsNull }
""" prelude it }

And create a specification with mocks: {"""

  object args1 extends Specification with Mockito {
    val mockedList = mock[List[String]]
    
    // stubbing using built-in anyInt() argument matcher
    mockedList.get(anyInt()) returns "element"

    // stubbing using hamcrest (let's say IsNull returns your own hamcrest matcher):
    mockedList.contains(argThat(new IsNull[String])) returns true
  }
""" prelude it }

Then, <ex>calling the mocked list with any argument must return "element"</ex>: 
  
{ "args1.mockedList.get(999)" snip it } 
{ >("element")}

and <ex>calling the mocked list @contains@ method with a valid argument must return "true" if the passed argument is null</ex>:
  
{ "args1.mockedList.contains(null)" snip it } 
{ >("true")}

<ex>It is also possible to verify that a mock was called with an argument matcher</ex>: {"""

  object args2 extends Specification with Mockito {
    args1.mockedList.get(999)
    args1.mockedList.get(isEq(999)) was called
  }
  args2.successes""" snip it }
  { >("example 1")}

<ex>Instead of Hamcrest matchers, a specs matcher can be used</ex>:{"""
  object args3 extends Specification with Mockito {
    val mockedList = mock[List[String]]
    mockedList.get(==(123)) returns "one"
  }
  args3.mockedList.get(123)
  """ snip it } 
  { >("one")}
  
  </wiki> isSus
}
