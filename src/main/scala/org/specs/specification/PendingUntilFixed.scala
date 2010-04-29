package org.specs.specification
import org.specs.execute._

/**
 * This trait allows examples to be marked as PENDING if their body is failing.
 * 
 * The usage is: <code>
 *  object s extends Specification with PendingUntilFixed { 
 *    "ex" in {       
 *       pendingUntilFixed { 1 must_== 2 }
 *     }
 *  } 
 * </code>
 * 
 * It is also possible to mark the example, or a whole sus as pendingUntilFixed with: <code>
 *  object s extends Specification with PendingUntilFixed { 
 *    "ex" in {       
 *       1 must_== 2
 *     } pendingUntilFixed
 *  } 
 *  object s2 extends Specification with PendingUntilFixed {
 *    // all examples will be pendingUntilFixed
 *    "sus" should {
 *      "ex1" in { 1 must_== 2 } 
 *      "ex2" in { 1 must_== 2 } 
 *    } pendingUntilFixed
 *  } 
 * </code>
 * If the example body is passing, then the example will fail with a message
 * warning the user that the pendingUntilFixed block can be removed
 */
trait PendingUntilFixed { outer =>
  /** implicit definition to add pendingUntilFixed ability to an example*/
  implicit def toPendingExample(e: Examples) = new PendingExample(e)
  class PendingExample(e: Examples) {
    def pendingUntilFixed = {
      def makePending(a: =>Any) = outer.pendingUntilFixed(a)
      e.aroundExpectations = Some(makePending(_)) 
      e
    }
  }
  def pendingUntilFixed(f: =>Any) { 
    val isPassing = 
      try { 
        f 
        true
      } 
      catch { case _ => false } 
      if (isPassing) 
        throw new FailureException("Fixed now. You should remove the 'pending until fixed' declaration") 
      else 
        throw new SkippedException("Pending until fixed") 
  } 
}
