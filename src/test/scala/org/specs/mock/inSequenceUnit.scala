package org.specs.mock
import org.specs.runner._
import org.specs.Sugar._
import org.specs.mock._
import scalacheck.Gen._
import org.specs.collection.ExtendedList._

class inSequenceTest extends JUnit3(inSequenceUnit)
object inSequenceUnit extends Specification with TestData with Scalacheck {
  "A protocol type 'inSequence'" should { usingBefore {() => clearCalls }
    "consume all if exp=m and rec=m" in {
      inAnyOrder.consume((e), (r)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed) 
      }
    }
    "consume the first exp if exp=m1, m2 and rec=m1" in {
      inAnyOrder.consume(List(e1, e2), (r1)) must verify { t:Result => val (exp, rec) = t
        e1.passes && !e2.passes && r1.consumed 
      }
    }
    "consume two exp if exp=m1, m2 and rec=m1, m2" in {
      inAnyOrder.consume(List(e1, e2), List(r1, r2)) must verify { t:Result => val (exp, rec) = t
        exp.forall(_.passes) && rec.forall(_.consumed) 
      }
    }
  }
  "not consume received calls if it is a strict sublist of expected calls" in {
    val lessReceivedCalls = receivedSizeIs(_ < _)
    lessReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
      inSequence.consume(expected, received)._2 must ((notBeEmpty).when(!expected.isEmpty) or 
                                                      (beEmpty).when(expected.isEmpty)) 
    }(set(maxSize->5))
  }
  "consume all received calls if it is a the same list of calls in the same order" in {
    val sameReceivedCalls = receivedSizeIs(_ == _)
    val emptyExpected: List[SpecifiedCall] = Nil
    val emptyReceived: List[ReceivedCall] = Nil

    sameReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
      inSequence.consume(expected, received) must be_==((emptyExpected, emptyReceived)).when(expected == received) 
    }(set(maxSize->5))
  }
  "consume all expected calls if they are a prefix of received calls" in {
    val moreReceivedCalls = receivedSizeIs(_ > _)
    moreReceivedCalls must pass { t: (List[ExpectedCall], List[ReceivedCall]) => val (expected, received) = t
      val receivedStartsWithExpected = received.map(_.method).startsWith(expected.map(_.method))
      val consumedReceived = inSequence.consume(expected, received)._2

      consumedReceived must (notBeEmpty.when(!received.isEmpty && receivedStartsWithExpected) and 
                             beEmpty.when(received.isEmpty)) 
    }(set(maxSize->5))
  }
}
