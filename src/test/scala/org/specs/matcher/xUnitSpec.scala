package org.specs.matcher
import org.specs.runner._

class xUnitSpecTest extends Runner(xUnitSpec) with JUnit
object xUnitSpec extends MatchersSpecification with xUnit {
  "the xUnit trait" should {
    "provide an assertTrue method" in {
      assertTrue(1 == 1)
      expectation { assertTrue(1 == 2) } must failWith("the value is false")
    }
    "provide an assertFalse method" in {
      assertFalse(1 == 2)
      expectation { assertFalse(1 == 1) } must failWith("the value is true")
    }
    "provide an assertEquals method" in {
      assertEquals(1, 1)
      assertEquals("s", "s")
      expectation { assertEquals(1, 2) } must failWith("'1' is not equal to '2'")
    }
    "provide an assertSame method" in {
      val s = "s" * 2
      assertSame(s, s)
      expectation { assertSame(s, "ss") } must failWith("'ss' is not the same as 'ss'")
    }
    "provide an assertNotSame method" in {
      val s = "s" * 2
      assertNotSame(s, "ss")
      expectation { assertNotSame(s, s) } must failWith("'ss' is the same as 'ss'")
    }
    "provide an assertNull method" in {
      assertNull(null)
      expectation { assertNull("1") } must failWith("'1' is not null")
    }
    "provide an assertNotNull method" in {
      assertNotNull(1)
      expectation { assertNotNull(null) } must failWith("the value is null")
    }
    "provide an assertArrayEquals method" in {
      assertArrayEquals(Array(1, 2, 3), Array(1, 2, 3))
      expectation { assertArrayEquals(Array(1, 2, 3), Array(1, 5, 6)) } must failWith("'2' is not equal to '5'; '3' is not equal to '6'")
    }
  }
}
