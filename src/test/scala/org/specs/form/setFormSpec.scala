/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.form
import org.specs.runner._

class setFormSpec extends org.specs.Specification with JUnit {
  case class Person(name: String, age: Int)
  case class PersonLine(name: String, age: Int) extends EntityLineForm[Person] {
    prop((_:Person).name)(name)
    prop((_:Person).age)(age)
  }
  val actual = Set(Person("Eric", 36), Person("Bob", 40))
  
  "A set form" should {
    "match all rows if there are the same number of rows than entities" in {
      val form = new DataTableSetForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 40    | { (name, age) =>
          tr(PersonLine(name, age))
        } 
      }
      (form.execute.toXhtml \\("tr")).toList must have size 4 
    }
    "define, matched/unmatched expected/actual rows" in {
      val form = new DataTableSetForm("Persons", actual) {
        "Name" | "Age" |
        "Eric" ! 36    | 
        "Bob"  ! 42    | { (name, age) =>
          tr(PersonLine(name, age))
        } 
      }
      form.execute.matchedLines aka "matched lines" must have size 2
      form.execute.matchedExpectedLines aka "matched expected lines" must have size 2
      form.execute.matchedActual aka "matched actual lines" must have size 2 
      form.execute.unmatchedExpectedLines aka "unmatched expected lines" must be empty
        
      form.execute.unmatchedActual aka "unmatched actual lines" must be empty
    }
    "report unmatched rows" in {
      val form = new SetForm(actual) {
        th2("Name", "Age")
        tr(PersonLine("Eric", 36)) 
        tr(PersonLine("Bob",  42)) 
      }
      (form.execute.toXhtml \\("tr")).toList must have size 4
    }
    "not match rows twice" in {
      val form = new SetForm(actual) {
        th2("Name", "Age")
        tr(PersonLine("Eric", 36)) 
        tr(PersonLine("Eric", 36)) 
      }
      form.execute.matchedLines aka "matched lines" must have size 1
      form.execute.matchedExpectedLines aka "matched expected lines" must have size 1
      form.execute.matchedActual aka "matched actual lines" must have size 1 
      form.execute.unmatchedExpectedLines aka "unmatched expected lines" must be empty
    }
  }
}
