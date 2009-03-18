package org.specs.mock
import org.specs.specification._
import org.specs.runner._

object returnValues extends returnValues
class returnValues extends LiterateSpecification("Return values") with MockitoSpecification {
<wiki>

h3. Return values

Optional ReturnValues can be used with mock[Class](ReturnValues). "ReturnValues":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/ReturnValues.html defines the return values of unstubbed invocations.

This implementation can be helpful when working with legacy code. Unstubbed methods often return null. If your code uses the object returned by an unstubbed call you get a NullPointerException. This implementation of ReturnValues makes unstubbed methods return SmartNull instead of null. SmartNull gives nicer exception message than NPE because it points out the line where unstubbed method was called. 
  You just click on the stack trace.

SmartNullReturnValues are the default for the Mockito trait in *specs* (it will be probably the default return values strategy in Mockito 2.0)
It first tries to return ordinary return values (see "MoreEmptyReturnValues":http://mockito.googlecode.com/svn/branches/1.7/javadoc/org/mockito/internal/returnvalues/MoreEmptyReturnValues.html) then it tries to return SmartNull. If the return type is final then plain null is returned.

{"""  
  import org.specs._
  import org.specs.mock.Mockito""" prelude it shh }
 
For Example: {"""

  class s1 extends Specification with Mockito {
    val list = mock[java.util.List[String]]
    val removed = list.remove(0)
  } """ snip it }

<ex>The returned value should yield a NullPointerException</ex>: 
  
{ "new s1().removed.toString" add it } 
{ >("NullPointerException")}

Unfortunately there is an issue with the SmartReturnValues in Scala: {"""

  class s2 extends Specification with Mockito {
    val list = smartMock[java.util.List[String]]
    val removed: String = list.remove(0)
  } """ snip it }

<ex>Accessing the returned value will yield a ClassCastException inside Mockito</ex>: 
  
{ "new s2().removed.toString" add it } 
{ >("java.lang.ClassCastException")}


  </wiki> isSus
}
