package org.specs.mock
import org.specs.specification._
import org.specs.runner._

object inOrder extends inOrder
class inOrder extends LiterateSpecification("In order calls") with MockitoSpecification {
<wiki>

h3. Checking that calls occur in the right order

{"""  
  import org.specs._
  import org.specs.mock.Mockito
  import org.mockito.Matchers.{ anyInt }
  import java.util.List """ prelude it shh }
 
With the following mocks: {"""

    class s extends Specification with Mockito {
      import java.util.List
      val m1 = mock[List[String]]
      val m2 = mock[List[String]]
    
      m1.get(0)
      m2.get(0)
   }
""" prelude it }

<ex>We can check that some calls happen in a given order</ex>: 
  
{ """new s {
      (m1.get(0) on m1).atLeastOnce then
      (m2.get(0) on m2)             were called.inOrder
     }.isOk""" snip it } 
{ >("true")}

<ex>If they don't, there should be a failure message</ex>: 
  
{ """new s {
      // use "theMethod" if you want to avoid implicit calls
      theMethod(m2.get(0)).on(m1) then
      theMethod(m1.get(0)).on(m2) were called.inOrder
     }.failures.first.getMessage""" snip it } 
{ >("The methods were not called in the right order: Wanted but not invoked: list.get(0)")}

<ex>Implicit def can also be removed for better readability</ex>: 
  
{ """new s {
      m1.add("1")
      m1.add("1")
      m2.add("2")

     (m1.add("1") on m1).times(2) then 
     (m2.add("2") on m2)          were called.inOrder
     }.isOk""" snip it } 
{ >("true")}




  </wiki> isSus
}
