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
}
