package org.specs.samples
import org.specs.runner._
import org.scalacheck.Commands

object stackSpecification extends StackSpec {
  "An empty stack" definedAs(empty) should { 
    "throw an exception when sent #top" in { stack: LimitedStack[Int] =>
      stack.top must throwA[NoSuchElementException]
    }
    "throw an exception when sent #pop" in { stack: LimitedStack[Int] =>
      stack.pop must throwA[NoSuchElementException]
    }
  }
  "A non-empty stack below full capacity" definedAs(nonEmpty) should {
    "not be empty" in { stack: SampleStack =>
      stack verifies { !_.isEmpty }
    }
    "return the top item when sent #top" in { stack: SampleStack =>
      stack.top mustBe stack.lastItemAdded
    }
    "not remove the top item when sent #top" in { stack: SampleStack =>
      stack.top mustBe stack.lastItemAdded
      stack.top mustBe stack.lastItemAdded
    }
    "remove the top item when sent #pop" in { stack: SampleStack =>
      stack.pop mustBe stack.lastItemAdded
      if (!stack.isEmpty)
        stack.top mustNotBe stack.lastItemAdded
    }
    "return the top item when sent #pop" in { stack: SampleStack =>
      stack.pop mustBe stack.lastItemAdded
    }
  }
  "A stack below full capacity" definedAs(belowCapacity) should {
    behave like "A non-empty stack below full capacity" 
    "add to the top when sent #push" in { stack: LimitedStack[Int] =>
      stack push 3
      stack.top mustBe 3
    }
  }
  "A full stack" definedAs(full) should { 
    behave like "A non-empty stack below full capacity" 
    "throw an exception when sent #push" in { stack: LimitedStack[Int] =>
      stack.push(11) must throwAn[Error]
    }
  }
}
class stackTest extends JUnit4(stackSpecification)

class StackSpec extends Specification {
  case class SampleStack(stackCapacity: Int, itemsNb: Int) extends LimitedStack[Int](stackCapacity) {
    def this(capacity: Int) = this(capacity, 0)
    var lastItemAdded = 0
    for (i <- 1 to itemsNb) { this += i; lastItemAdded = i } 
  }
  case class StackContext(capacity: Int, itemsNb: Int) extends SystemContext[SampleStack] {
    def this(capacity: Int) = this(capacity, 0)
    def newSystem = SampleStack(capacity, itemsNb)
  }
  val empty = new StackContext(10)
  val full = StackContext(10, 10)
  val nonEmpty = StackContext(10, 1)
  val belowCapacity = StackContext(10, 3)
}

class LimitedStack[T](val capacity: Int) extends scala.collection.mutable.Stack[T] {
  override def push(a: T*) = {
    if (size >= capacity) throw new Error("full stack") else this ++= a
  }
}