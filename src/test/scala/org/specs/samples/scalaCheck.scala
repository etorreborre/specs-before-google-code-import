package org.specs.samples
import org.specs._
import org.specs.specification._
import org.specs.matcher._

object StringSpecification extends Specification("String") with ScalaCheck {
  
   "startsWith" verifies ((a: String, b: String) => (a + b).startsWith(a)).set(minTestsOk->250)
   "endsWith" verifies { (a: String, b: String) => (a + b).endsWith(b) }
   "concat" verifies { (a: String, b: String) => (a + b).length == a.length + b.length }
   "substring" verifies { (a: String, b: String) => (a + b).substring(a.length) == b }
   "substring" verifies { (a: String, b: String, c: String) => (a + b + c).substring(a.length, a.length + b.length) == b } 

}
 