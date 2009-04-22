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

class EntityLineForm[T] extends LineForm {
  var entity: Option[T] = None
  /** add a new LineProp to that line */
  def prop[S](s: String, f:(T => S)): LineProp[S] = {
    lazy val actual: Option[S] = entity.map(f(_))
    val p = new LineProp(label, None, actual, Some(MatcherConstraint((m:org.specs.matcher.Matcher[S]) => actual.map(_ must m))))
    lineProperties.append(p)
    add(p)
    p
  }
  /** add a new LineProp to that line */
  def prop[S](f:(T => S)): LineProp[S] = prop("", f) 
  /** in that case a LineField is modeled as a commented line prop */
  def field[S](s: String, f:(T => S)): LineProp[S] = prop(s, f).comment
  /** in that case a LineField is modeled as a commented line prop */
  def field[S](f:(T => S)): LineProp[S] = field("", f) 
  def entityIs(a: T): this.type = entityIs(Some(a))
  def entityIs(a: Option[T]): this.type = { entity = a; this }
}