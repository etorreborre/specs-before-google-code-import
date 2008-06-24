package org.specs.specification

object literalSpec extends Fixtures { <text> 
  A literal specification is a text embedded in xml tags.
  A literal specification can execute code by enclosing it in accolades: { (1 + 1).shh }
 
  However, <ex>an assertion can be included in a literal specification as part of an example</ex>, like this:
{""" 
     { "1 must be 1" in {1 must_== 1} }    """}{exampleOk}
 
  or <ex>with an "ex" xml tag</ex>, like that:
{""" 
     <ex>1 must be 1</ex> { 1 must_== 1  }   """}{taggedExample}
 
  <ex>It is possible to mark an example as not implemented yet</ex>:
{""" 
     <ex> this example is not yet implemented </ex> { notImplemented }   """} {notImplementedExample}
    
  In that case, the example will be added but marked as skipped
</text> isSut
}
trait Fixtures extends LiteralSpecification {
   object example1 extends LiteralSpecification  {
     <t>{"1 must be 1" in {1 must_== 1}}</t> isSut  }
   object example2 extends LiteralSpecification  {
     <t><ex>1 must be 1</ex> { 1 must_== 1  } </t> isSut  }
   object example3 extends LiteralSpecification  {
     <t><ex> this example is not yet implemented </ex> { notImplemented }</t> isSut  }
   def exampleOk = checkSuccess(example1)
   def taggedExample = checkSuccess(example2)
   def notImplementedExample = checkSkipped(example3)
   def checkSuccess(s: Specification) = check(s.suts.flatMap(_.examples).flatMap(_.failures).size must_== 0)
   def checkSkipped(s: Specification) = check(s.suts.flatMap(_.examples).flatMap(_.skipped).size must_== 1)
}
class LiteralSpecTest extends org.specs.runner.JUnit4(literalSpec)
