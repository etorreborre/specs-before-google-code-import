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
import spex._
import Field._

class fieldSpec extends Specification {
  "A Field" should {
    "have a toString method displaying the label and value" in {
      Field("label", "value").toString must_== "label: value"
    }
    "a toString method formatting Doubles properly" in {
      Field("label", 1.2345).toString must_== "label: 1.2345"
    }
    "allow a to set a success style attribute to decorate the value" in {
      val f = Field("Result", 1.12).successValue.toXhtml(1)
      f must ==/(<td class="success">1.12</td>)
    }
    "allow a to set both a success style attribute and a bold style to decorate the value" in {
      val f = Field("Result", 1.12).boldValue.successValue.toXhtml(1)
      f must ==/(<td class="success"><b>1.12</b></td>)
    }
    "allow a to set an attribute, like bgcolor, on a value cell" in {
      val f = Field("Result", 1.12).valueCellAttribute("bgcolor", "#FF6699").toXhtml(1)
      f must ==/(<td class="info" bgcolor="#FF6699">1.12</td>)
    }
    "allow a to set the bg color of a cell, setting the class attribute to 'none'" in {
      val f = Field("Result", 1.12).valueBgcolor("#FF6699").toXhtml(1)
      f must ==/(<td class="none" bgcolor="#FF6699">1.12</td>)
    }
  }
  "A Field" can {
    "concanate other fields" in {
      val f1 = Field("f1", "value1")
      val f2 = Field("f2", "value2")

      Field("label", f1, f2).toString must_== "label: value1/value2"
    }
    "concanate other fields with a custom separator" in {
      val f1 = Field("f1", "value1")
      val f2 = Field("f2", "value2")

      Field("label", ", ", f1, f2).toString must_== "label: value1, value2"
    }
  }
}
