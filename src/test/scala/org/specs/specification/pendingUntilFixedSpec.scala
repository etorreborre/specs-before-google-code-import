package org.specs.specification
import org.specs._

class pendingUntilFixedSpec extends SpecificationWithJUnit {
   "A specification extending PendingUntilFixed" should {
     "mark failing examples as skipped" in {
       object s extends Specification with PendingUntilFixed { 
         "ex" in {       
           pendingUntilFixed { 1 must_== 2 }
         }
       } 
       s.skipped must have size(1)
     }
     "mark passing examples as failed - with a message to remove the pending block" in {
       object s extends Specification with PendingUntilFixed { 
         "ex" in {       
           pendingUntilFixed { 1 must_== 1 }
         }
       } 
       s.skipped must be empty;
       s.failures must have size(1)
       s.failures(0).getMessage must_== "Fixed now. You should remove the 'pending until fixed' declaration"
     }
   }
   "A specification extending PendingUntilFixed" can {
     "use the pendingUntilFixed method on Examples to set the example body as PENDING" in {
       object s extends Specification with PendingUntilFixed { 
         "ex" in { 
           1 must_== 2 
         } pendingUntilFixed
       } 
       s.skipped must have size(1)
     }
     "use the pendingUntilFixed method on a sus to set the examples PENDING" in {
       object s extends Specification with PendingUntilFixed {
         "sus" should {
           "ex" in { 1 must_== 2 } 
           "ex2" in { 1 must_== 2 }
         } pendingUntilFixed
       } 
       s.skipped must have size(2)
     }
   }
}
