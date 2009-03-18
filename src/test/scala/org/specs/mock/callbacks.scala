package org.specs.mock
import org.specs.specification._
import org.specs.runner._

object callbacks extends callbacks
class callbacks extends LiterateSpecification("Stubbing with callbacks") with MockitoSpecification {
<wiki>

h3. Stubbing with callbacks

Allows stubbing with generic "Answer":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/stubbing/Answer.html interface.
Yet another controversial feature which was not included in Mockito originally. 
We recommend using simple stubbing with @returns@ or @throws@ only. Those two should be just enough to test/test-drive any clean and simple code.
{"""  
  import org.specs._
  import org.specs.mock.Mockito
  import org.mockito.Matchers.{ anyInt }
  import java.util.List """ prelude it shh }
 
Here is a specification where the stubbed return values depend on the method parameters: {"""

  object s extends Specification with Mockito {
    val mockedList = mock[List[String]]
    
    // stubbing using built-in anyInt() argument matcher
    mockedList.get(anyInt()) answers { i => "The parameter is " + i.toString } 
  }
""" snip it }

<ex>The function passed to @returns@ will be called with each parameter passed to the stubbed method</ex>: 
  
{ "s.mockedList.get(0)" add it } 
{ >("The parameter is 0")}

<ex>The second call returns a different value</ex>:
  
{ "s.mockedList.get(1)" add it } 
{ >("The parameter is 1")}

  </wiki> isSus
}
