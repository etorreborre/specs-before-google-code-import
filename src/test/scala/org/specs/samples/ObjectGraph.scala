package org.specs.samples
import org.specs.runner._
import org.specs.matcher._
import org.specs.specification._

class objectGraphSpec extends Specification with ObjectGraphMatchers with JUnit {
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
    var solobar: Bar = null
    val bars = mutable.Set[Bar]()
  }
  case class Bar(val id: Long)
}

trait ObjectGraphMatchers extends ObjectGraph with Matchers {
  case class matchFoo(foo: Foo) extends Matcher[Foo] {
    def apply(other: => Foo) = {
      ((beEqualTo(_:String)) ^^^ ((_:Foo).name) and
       (matchOptionalBar(_)) ^^^ ((_:Foo).solobar) and
       (matchBar(_)).toSet ^^^ ((_:Foo).bars))(foo)(other)
    }
  }
  case class matchOptionalBar(bar: Bar) extends Matcher[Bar] {
    def apply(other: => Bar) = {
      ((beAsNullAs(_:Bar)) or
      ((matchBar(_))))(bar)(other)
    }
  }
  case class matchBar(bar: Bar) extends Matcher[Bar] {
    def apply(other: => Bar) = {
      ((beEqualTo(_:Long)) ^^^ ((_: Bar).id))(bar)(other)
    }
  }
}
