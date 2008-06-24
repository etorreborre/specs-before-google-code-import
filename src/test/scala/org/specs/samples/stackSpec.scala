package org.specs.samples
import org.specs.runner._
import org.scalacheck.Commands

class stackTest extends JUnit4(stackSpecification)
object stackSpecification extends StackSpec {
  "An empty stack" ->-(empty) should { 
    "throw an exception when sent #top" in {
      stack.top must throwA(new NoSuchElementException)
    }
    "throw an exception when sent #pop" in {
      stack.pop must throwA(new NoSuchElementException)
    }
  }
  "A non-empty stack below full capacity" ->-(nonEmpty) should {
    "not be empty" in {
      stack verifies { !_.isEmpty }
    }
    "return the top item when sent #top" in {
      stack.top mustBe lastItemAdded
    }
    "not remove the top item when sent #top" in {
      stack.top mustBe lastItemAdded
      stack.top mustBe lastItemAdded
    }
    "remove the top item when sent #pop" in {
      stack.pop mustBe lastItemAdded
      if (!stack.isEmpty)
        stack.top mustNotBe lastItemAdded
    }
    "return the top item when sent #pop" in {
      stack.pop mustBe lastItemAdded
    }
  }
  "A stack below full capacity" ->-(belowCapacity) should {
    behave like "A non-empty stack below full capacity" 
    "add to the top when sent #push" in {
      stack push 3
      stack.top mustBe 3
    }
  }
  "A full stack" ->-(full) should { 
    behave like "A non-empty stack below full capacity" 
    "throw an exception when sent #push" in {
      stack.push(11) must throwA(new Error)
    }
  }
}
class StackSpec extends Specification {
  val stack = new LimitedStack[Int](10)
  var lastItemAdded = 0
  def createStack(itemsNb: Int) = { stack.clear; for (i <- 1 to itemsNb) stack += i; lastItemAdded = stack.top } 
  val empty = beforeContext(stack.clear)
  val full = beforeContext(createStack(stack.capacity))
  val nonEmpty = beforeContext(createStack(3))
  val belowCapacity = beforeContext(createStack(3))
}

class LimitedStack[T](val capacity: Int) extends scala.collection.mutable.Stack[T] {
  override def push(a: T*) = {
    if (size >= capacity) throw new Error("full stack") else this ++= a
  }
}