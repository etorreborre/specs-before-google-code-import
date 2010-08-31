package org.specs.runner
import org.specs.Specification
/**
 * The SpecsHolder trait is used by any class providing access to a sequence of specifications
 */
private[specs] trait SpecsHolder {
  val specs: Seq[Specification]
}


