package org.specs.mock
import org.specs.Sugar._
import org.specs.collection.ExtendedList._

case object inSequence extends inSequence(exactlyN(1))

/**
 * The <code>inSequence</code> protocol type will try to consume expected calls
 * in sequence. It will not consume unexpected calls.<br>
 * It accepts a <code>repetition</code> parameter specifying how many expected calls are supposed to happen:<ul>
 * <li>exactlyN(2): exactly 2 times
 * <li>atLeast(2): at least 2 times
 * <li>atMost(2): at most 2 times</ul>
 */
class inSequence(repetition: CallConstraint) extends ProtocolType(repetition) {

 /**
  * @return a String specifying the constraints of this protocol: e.g. "in sequence 2 of:".
  * If it is exactly one, returns "in sequence"
  */
  def constraints = {
     repetition match{
       case exactlyN(n) if (n == 1) =>  "in sequence"
       case _ => "in sequence " + repetition.expectation
     }
   }
  
  /**
   * Tries to match expected calls with received calls in sequence
   * until the <code>repetition</code> parameter is satisfied.<br>
   * Before doing so, it sets the repetition number on expected calls, so that they
   * know when to stop matching received calls (especially for atLeast and atMost constraints).<br>
   * If consumed received calls are not in the same order as their respective expected calls
   * it resets them so that the received calls are not consumed and the expected calls are not passed
   * @return the list of expected calls and the list of received calls
   */
  def consume(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    exp.foreach(_.repetition = repetition)
    var n = 0
    do {    
      exp foreach (_.consume(rec))
      if (hasConsumedCallsNotInSequenceWithPassedCalls(exp, rec)) {
        rec foreach (_.consumedBy = None)
        exp foreach {_.clear}
      }
      n = n + 1
    } while (!repetition.verifies(n))
    (exp, rec)
  }
   
  private def hasConsumedCallsNotInSequenceWithPassedCalls(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    rec.exists(r1 =>
      r1.consumed && rec.exists(r2 =>
        r2.consumed && 
           (exp.indexOf(r1.consumedBy.get) > exp.indexOf(r2.consumedBy.get)) &&
           (rec.indexOf(r1) < rec.indexOf(r2))  
      )
    )
 }
}
