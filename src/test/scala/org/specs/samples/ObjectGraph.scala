package org.specs.samples
import org.specs.runner._
import org.specs.matcher._
import org.specs.specification._

class ObjectGraphRunner extends Runner(ObjectGraphSpec) with JUnit
object ObjectGraphSpec extends Specification with ObjectGraphMatchers {
  "The following object graphs" should {
    "match when they are the same" in {
      val foo = new Foo("2")
      foo.bars += new Bar(33) 
      foo.bars += new Bar(12) 

      val foo2 = new Foo("2") 
      foo2.bars += new Bar(12) 
      foo2.bars += new Bar(33) 
  
      foo must matchFoo(foo2)
    }
  }
}

trait ObjectGraph { 
  import scala.collection.mutable 

  case class Foo(val name:String) { 
    var solobar: Bar = _ 
    val bars = mutable.Set[Bar]() 
  } 
  
  case class Bar(val id: Long)
}

trait ObjectGraphMatchers extends ObjectGraph with Matchers {
  case class matchFoo(foo: Foo) extends Matcher[Foo] {
    def apply(other: => Foo) = {
      ((beEqual(_:String)) ^^^ ((f:Foo) => f.name) and
       (matchBar(_)) ^^^ ((f: Foo) => foo.solobar) and
       (matchBar(_)).toSet ^^^ ((f: Foo) => foo.bars))(foo)(other)
    }
  }
  case class matchBar(bar: Bar) extends Matcher[Bar] {
    def apply(other: => Bar) = {
      ((beAlsoNull(_:Bar)) or 
      ((beEqual(_:Long)) ^^^ ((b: Bar) => b.id)))(bar)(other)
    }
  }
}
