package org.specs.mock
import org.specs.runner._
import org.specs.Sugar._
import org.specs.mock._

object mockerUnitRunner extends ConsoleRunner(mockerUnit)
class mockerUnitTest extends JUnit3(mockerUnit)
object mockerUnit extends Specification with Sugar with ProtocolTypes {
  object mocker extends Mocker
  class MockedClass { def method = (); def a = (); def b = (); def c = () }
  "A mocker" should {
    usingBefore {() => mocker.protocol.clear}
    "create a protocol when expecting calls" in {
      val protocol = mocker.expect {}
      protocol mustNotBe null
    }
    "add a new expected call when recording calls" in {
      val mock = new MockedClass() { override def method = mocker.record }
      val protocol = mocker.expect { mock.method }
      
      protocol verifies(_.isSpecified)
      protocol.definition mustNotBe null
      protocol.definition.expectedCalls must beLike { case List(ExpectedCall(_)) => ok }
    }
    "add a new received call when receiving calls" in {
      val mock = new MockedClass() { override def method = mocker.record }
      val protocol = mocker.expect { mock.method }
      mock.method
      
      protocol.receivedCalls must beLike { case List(ReceivedCall(_)) => ok }
    }
    "have a failure when not receiving an expected call" in {
      val mock = new MockedClass() { override def method = mocker.record }
      val protocol = mocker.expect(inAnyOrder) { mock.method }

      protocol.failures must beMatching("Expected in any order .*. Received none")
    }
    "include a protocol def inside a protocol def if expectations are nested" in {
      val mock = new MockedClass() { override def a = mocker.record; override def b = mocker.record; override def c = mocker.record }
      val protocol = mocker.expect {
        mock.a
        mocker.expect { mock.b } 
        mock.c
      }
      protocol.definition must beLike { case ProtocolDef(inAnyOrder, List(ExpectedCall(_), 
                                                                           ProtocolDef(_, _),
                                                                           ExpectedCall(_))) => ok }
    }
    "accept nested protocol defs using different protocol types: anyOf 1.of{method}; 2.of {method}" in {
      val mock = new MockedClass() { override def a = mocker.record; override def b = mocker.record; override def c = mocker.record }
      val protocol = mocker.expect {
        mocker.expect(oneOf) { mock.a } 
        mocker.expect(twoOf) { mock.b } 
      }
      protocol.definition must beLike { case ProtocolDef(inAnyOrder, List(ProtocolDef(x, _),
                                                                          ProtocolDef(y, _))) => (x, y) == (oneOf, twoOf) }
    }
  }
}
