package org.specs.samples
import org.specs._
import org.specs.runner._
import org.specs.specification._
import org.specs.matcher._

class stringSpec extends Specification("String") with ScalaCheck with JUnit {

   "startsWith" verifies ((a: String, b: String) => (a + b).startsWith(a)).set(minTestsOk->25)
   "endsWith" verifies { (a: String, b: String) => (a + b).endsWith(b) }
   "concat" verifies { (a: String, b: String) => (a + b).length == a.length + b.length }
   "substring" verifies { (a: String, b: String) => (a + b).substring(a.length) == b }

}
