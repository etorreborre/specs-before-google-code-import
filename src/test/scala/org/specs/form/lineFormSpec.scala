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

class lineFormSpec extends spex.Specification {
  val lineForm = new LineForm {
        prop("First Name", "Hello")
        prop("Last Name", "World")
      }
  "a line form" should {
    "display all values when rendering its Xhtml method" in {
      lineForm.toHtml.toString must include("Hello") and include("World")
    }
    "have no labels displayed when rendering its Xhtml method" in {
      lineForm.toHtml.toString must not include("First Name")
    }
    "return a header with the properties labels" in {
      lineForm.header.toString must include("First Name") and include("Last Name")
    }
    "return a row containing the properties values instead when queried for rows" in {
      lineForm.rows must have size 1
      lineForm.rows aka "multiple rows query" must have size 1
    }
  }
}
