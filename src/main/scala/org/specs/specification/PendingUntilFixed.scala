package org.specs.specification
import org.specs.execute._

trait PendingUntilFixed { this: BaseSpecification =>
  def pendingUntilFixed(f: => Unit) { 
    val isFailing = 
      try { 
        f 
        false 
      } 
      catch { case _ => true } 
      if (isFailing) 
        throw new SkippedException("Pending until fixed") 
      else 
        throw new FailureException("Fixed now. You should remove the 'pending until fixed' declaration") 
  } 
}
