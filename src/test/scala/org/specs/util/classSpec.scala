/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
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
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.util
import org.specs.util.Classes._
import org.specs.specification.BaseSpecification

class classSpec extends spex.Specification {
  "the class name of a full name with package should only return the last name" in {
    className("org.specs.MyName") must_== "MyName"
  }
  "the class name of an internal class should only return the last name" in {
    class ThisClassName
    className(classOf[ThisClassName].getName) must_== "ThisClassName"
  }
  "the class name of an Int should be Integer" in {
    getClassName(1) must_== "Integer"
  }
  "the class name of a String should be String" in {
    getClassName("1") must_== "String"
  }
  "create object" should {
    "return an instance if the instance can be created" in {
      create[String]("java.lang.String") must be right(new String)
    }
    "return an exception if the class can't be loaded" in {
      create[String]("missing") must beLeft("java.lang.ClassNotFoundException: missing") ^^ (_.left.map(_.toString))
    }
  }
  "A tryToCreateObject function" should {
    "try to create an instance using the first available constructor" in {
      class spec extends spex.Specification
      val s = new spec
      tryToCreateObject[BaseSpecification](s.getClass.getName, true, true) must not be none
    }
  }
}
