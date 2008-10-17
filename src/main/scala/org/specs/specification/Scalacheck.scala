package org.specs
import org.specs.matcher.{ScalacheckMatchers, ScalacheckParameters}
import org.scalacheck.Prop._
import org.scalacheck.Shrink
import org.scalacheck.Arbitrary
import org.specs.matcher.Parameters
import org.specs.specification._

/**
 * This trait can be mixed with a specification to allow the use of Scalacheck in a specification
 */
trait Scalacheck extends ScalacheckMatchers with ScalacheckParameters { this: ExpectableFactory with SpecificationStructure =>
     implicit def toverifies(e: String) = new Object {
     def verifies[A1](f: (A1) => Boolean)(implicit a1: Arbitrary[A1],
                                                 s1: Shrink[A1], params: Parameters) = forExample(e) in { property(f) must pass(params) }
     def verifies[A1, A2](f: (A1, A2) => Boolean)(implicit a1: Arbitrary[A1], 
                                                         a2: Arbitrary[A2],
                                                         s1: Shrink[A1], 
                                                         s2: Shrink[A2], params: Parameters) = forExample(e) in { property(f) must pass(params) }
     def verifies[A1, A2, A3](f: (A1, A2, A3) => Boolean)(implicit a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], params: Parameters) = forExample(e) in { property(f) must pass(params) }
     def verifies[A1, A2, A3, A4](f: (A1, A2, A3, A4) => Boolean)(implicit a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             params: Parameters) = forExample(e) in { property(f) must pass(params) }
     def verifies[A1, A2, A3, A4, A5](f: (A1, A2, A3, A4, A5) => Boolean)(implicit a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5], 
                                                             params: Parameters) = forExample(e) in { property(f) must pass(params) }
     def verifies[A1, A2, A3, A4, A5, A6](f: (A1, A2, A3, A4, A5, A6) => Boolean)(implicit a1: Arbitrary[A1], 
                                                             a2: Arbitrary[A2], 
                                                             a3: Arbitrary[A3],
                                                             a4: Arbitrary[A4],
                                                             a5: Arbitrary[A5],
                                                             a6: Arbitrary[A6],
                                                             s1: Shrink[A1], 
                                                             s2: Shrink[A2], 
                                                             s3: Shrink[A3], 
                                                             s4: Shrink[A4], 
                                                             s5: Shrink[A5], 
                                                             s6: Shrink[A6], 
                                                             params: Parameters) = forExample(e) in { property(f) must pass(params) }
   }

}
