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
package org.specs.specification
import org.specs.util.Classes

class executionPathSpec extends spex.Specification with Classes {
  shareVariables()
  "An activation path" should {
    "have a ::: method to append 2 paths" in {
      (ActivationPath(List(0, 1)) ::: ActivationPath(List(2, 3))) must_== ActivationPath(List(0, 1, 2, 3)) 
    }
    "have a pathFromRoot method returning the path of the element in the tree of elements" in {
      s.childrenNodes(0).childrenNodes(0).childrenNodes(0).pathFromRoot must_== ActivationPath(List(0, 0, 0, 0))
    }
    "have a pathFromRoot method returning the path of the element in the tree of elements" in {
      s.childrenNodes(0).childrenNodes(1).childrenNodes(1).pathFromRoot must_== ActivationPath(List(0, 0, 1, 1))
    }
  }
  "An activation node" should {
    "not be activated if its activation path is empty" in {
      new ActivationNode(ActivationPath(Nil)).activated must beFalse
    }
    "be activated if its activation path is not empty" in {
      new ActivationNode(ActivationPath(1)).activated must beTrue
    }
  }
  val spec = new Specification {
    "this system" should {
      "have one ex" in {
        "with one sub11" in { 1 must_== 1 }
        "with one sub12" in { 1 must_== 1 }
      }
      "have two ex" in {
        "with one sub21" in { 1 must_== 1 }
        "with one sub22" in { 1 must_== 1 }
      }
    }
  }
  "An Example" should {
    "not create subexamples until asked for it" in {
      val e = new Example("", this)
      var spyCalled = false
      e.in { 
        spyCalled = true
        new Example("subex", this) 
      }
      spyCalled must beFalse
      e.subExamples
      spyCalled must beTrue
    }
    "have a getExample method returning the subexample at a given path" in {
      val sys = spec.systems(0)
      val ex = sys.examples(0)
      val subex = ex.subExamples(1)
      ex.getExample(ActivationPath(1)) must_==(Some(subex))
    }
  }
  "A Sus" should {
    "have a getExample method returning the example at a given path" in {
      val sus = spec.systems(0)
      println("it is "+sus.description)
      println("it has "+sus.examples)
      val ex = sus.examples(1)
      sus.getExample(ActivationPath(1)) must_== Some(ex)
    }
  }
  "A specification" should {
    "be activated by default" in {
      s.activated must beTrue
    }
    "have all activation paths set to a subexample" in {
      spec.pathFromRoot must_== ActivationPath(0)
      spec.systems(0).pathFromRoot must_== ActivationPath(List(0, 0))
      spec.systems(0).examples(0).pathFromRoot must_== ActivationPath(List(0, 0, 0))
      spec.systems(0).examples(0).subExamples(1).pathFromRoot must_== ActivationPath(List(0, 0, 0, 1))
    }
    "have a getExample method returning the example at a given path" in {
      val subex = spec.systems(0).examples(0).subExamples(1)
      spec.getExample(subex.pathFromRoot) must_== Some(subex)
    }
  }
}
class ActivationNode(var activationPath: ActivationPath) extends ActivationTree[ActivationNode] {
  def this() = this(ActivationPath(List(0)))
  def activated = !activationPath.path.isEmpty
  def deactivate = activationPath = ActivationPath(Nil)
}
trait ActivationTree[T <: ActivationTree[T]] {
  private[specification] var childrenNodes: List[T] = Nil
  private[specification] var parentNode: Option[ActivationTree[T]] = None
  
  def pathFromRoot: ActivationPath = parentNode match {
    case None => new ActivationPath(0)
    case Some(p) => p.pathFromRoot ::: new ActivationPath(p.childrenNodes.indexOf(this))
  } 
    
  def addChild(t: T) = {
    childrenNodes = childrenNodes ::: List(t)
    t.parentNode = Some(this)
  }
}
case class ActivationPath(path: List[Int]) {
  def this(i: Int) = this(List(i))
  def :::(other: ActivationPath) = ActivationPath(other.path ::: path)
}
case class Ex(label: String) extends ActivationNode {
  def add(ex: Ex) = addChild(ex)
}
case class Sys() extends ActivationNode {
  val label = "sys"
  def add(ex: Ex) = addChild(ex)
}  
case class Spec() extends ActivationNode {
  val label = "spec"
  def add(sys: Sys) = addChild(sys)
}
object s extends activatedSpec
class activatedSpec extends Spec {
  add(new Sys() {
    add {
      new Ex("ex1") {
        add(new Ex("sub11")) 
        add(new Ex("sub12")) 
      }
    }
    add {
      new Ex("ex2") {
        add(new Ex("sub21")) 
        add(new Ex("sub22")) 
      }
    }
  })
}
