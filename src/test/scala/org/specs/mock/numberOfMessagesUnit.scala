package org.specs.mock;
import org.specs.runner._
import org.specs.Sugar._
import org.specs.mock._
import scalacheck.Gen._
import org.specs.collection.ExtendedList._
import org.specs.Scalacheck

class numberOfMessagesTest extends JUnit3(numberOfMessagesUnit)
object numberOfMessagesUnit extends Specification with TestData with Scalacheck {
  "A protocol type 'numberOfMessages'" should { usingBefore {() => clearCalls }
    "exactly 2: consume all if exp=m and rec=m, m" in {
      new inAnyOrder(exactlyN(2)).consume((e), List(r, rprime)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed) 
      }
    }
    "exactly 2: not pass the expected call if exp=m and rec=m, but consume the received call" in {
      new inAnyOrder(exactlyN(2)).consume(List(e), List(r)) must_== (List(e), List(r))
      new inAnyOrder(exactlyN(2)).consume((e), List(r)) must verify { t:Result => val (exp, rec) = t
        exp.forall(!_.passes) && rec.forall(_.consumed) 
      }
    }
  }
  "An exactly(N) protocol type" should {
    val exactly2 = new inAnyOrder(exactlyN(2))
    "not pass expected calls at all if there are less calls than expected" in {
      val lessReceivedCalls = receivedSizeIs(_ < _)
      lessReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
        exactly2.consume(expected, received)
        expected.forall(_.passes) must be(false).unless(expected.isEmpty || received.isEmpty)
      }(set(maxSize->5))
    }
    "consume all expected and received calls if it is a multiple of the expected calls" in {
      sameCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
        exactly2.consume(expected, received:::(received.map((r: ReceivedCall)=>ReceivedCall(r.method))))
        expected.forall(_.passes) must be(true).unless(expected.isEmpty || received.isEmpty)
      }(set(maxSize->5))
    }
    "not pass the expected calls if the received calls are not an exact multiple of the expected calls" in {
      sameCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, same) = t
        exactly2.consume(expected, same:::same:::same)
        expected.forall(_.passes) must be(false).unless(expected.isEmpty || same.isEmpty)
      }(set(maxSize->5))
    }
  }
}
