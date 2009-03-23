package org.specs.samples
import org.specs.runner._
import org.scalacheck.Commands

class implicitStackSpec extends StackSpecification with JUnit with SystemContexts {
  "An empty stack" should {
    implicit val stackIsEmpty = systemContext { new SampleStack(10) }
    
    "throw an exception when sent #top" into { stack: SampleStack =>
      stack.top must throwA[NoSuchElementException]
    }
    "throw an exception when sent #pop" into { stack: SampleStack =>
      stack.pop must throwA[NoSuchElementException]
    }
  }
  "A non-empty stack below full capacity" should {
    implicit val nonEmptyStack = systemContext { new SampleStack(10, 5) }

    "not be empty".withA(nonEmptyStack) { stack =>
      stack verifies { !_.isEmpty }
    }
    "return the top item when sent #top" into { s: SampleStack =>
      s.top mustBe s.lastItemAdded
    }
    "not remove the top item when sent #top" into { stack: SampleStack =>
      stack.top mustBe stack.lastItemAdded
      stack.top mustBe stack.lastItemAdded
    }
    "remove the top item when sent #pop" into { stack: SampleStack =>
      stack.pop mustBe stack.lastItemAdded
      if (!stack.isEmpty)
        stack.top mustNotBe stack.lastItemAdded
    }
    "return the top item when sent #pop" into { stack: SampleStack =>
      stack.pop mustBe stack.lastItemAdded
    }
  }
  "A stack below full capacity" should {
    implicit val stackIsNotEmpty = systemContext { new SampleStack(10, 5) }

    behave like "A non-empty stack below full capacity"

    "add to the top when sent #push" into { stack: SampleStack =>
      stack push 3
      stack.top mustBe 3
    }
  }
  "A full stack" should {
    implicit val fullStack = systemContext { new SampleStack(10, 10) }
    
    behave like "A non-empty stack below full capacity"
    "throw an exception when sent #push" into { stack: SampleStack =>
      stack.push(11) must throwAn[Error]
    }
  }
}