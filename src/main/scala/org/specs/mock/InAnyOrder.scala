package org.specs.mock
import org.specs.collection.ExtendedList._
import org.specs.Sugar._

case object inAnyOrder extends inAnyOrder(exactlyN(1))

/**
 * The <code>inAnyOrder</code> protocol type will try to consume expected calls
 * in any order. It will not consume unexpected calls.<br>
 * It accepts a <code>repetition</code> parameter specifying how many expected calls are supposed to happen:<ul>
 * <li>exactlyN(2): exactly 2 times
 * <li>atLeast(2): at least 2 times
 * <li>atMost(2): at most 2 times</ul>
 */
class inAnyOrder(val repetition: CallConstraint) extends ProtocolType(repetition) {
   /**
    * @return a String specifying the constraints of this protocol. If it is exactly one, returns "in any order"
    */
   def constraints = {
    repetition match{
      case exactlyN(n) if (n == 1) => "in any order" 
      case _ => repetition.expectation
    }
  }
  /**
   * Tries to match expected calls with received calls in any order
   * until the <code>repetition</code> parameter is satisfied<br>
   * Before doing so, it sets the repetition number on expected calls, so that they
   * know when to stop matching received calls (especially for atLeast and atMost constraints).
   * @return the list of expected calls and the list of received calls
   */
  def consume(exp: List[SpecifiedCall], rec: List[ReceivedCall]) = {
    exp.foreach(_.repetition = repetition)
    var n = 0
    do {    
      exp foreach (_.consume(rec))
      n = n + 1
    } while (!repetition.verifies(n))
    (exp, rec)
  }
}
