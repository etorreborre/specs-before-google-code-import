package org.specs.matcher
import org.specs.runner._

class xUnitSpecTest extends Runner(xUnitSpec) with JUnit
object xUnitSpec extends MatchersSpecification with xUnit {
  "the xUnit trait" should {
    "provide an assertTrue method" in {
      assertTrue(1 == 1)
      assertion { assertTrue(1 == 2) } must failWith("the value is false")
    }
    "provide an assertFalse method" in {
      assertFalse(1 == 2)
      assertion { assertFalse(1 == 1) } must failWith("the value is true")
    }
    "provide an assertEquals method" in {
      assertEquals(1, 1)
      assertEquals("s", "s")
      assertion { assertEquals(1, 2) } must failWith("'1' is not equal to '2'")
    }
    "provide an assertSame method" in {
      val s = "s" * 2
      assertSame(s, s)
      assertion { assertSame(s, "ss") } must failWith("'ss' is not the same as 'ss'")
    }
    "provide an assertNotSame method" in {
      val s = "s" * 2
      assertNotSame(s, "ss")
      assertion { assertNotSame(s, s) } must failWith("'ss' is the same as 'ss'")
    }
    "provide an assertNull method" in {
      assertNull(null)
      assertion { assertNull("1") } must failWith("'1' is not null")
    }
    "provide an assertNotNull method" in {
      assertNotNull(1)
      assertion { assertNotNull(null) } must failWith("the value is null")
    }
    "provide an assertArrayEquals method" in {
      assertArrayEquals(Array(1, 2, 3), Array(1, 2, 3))
      assertion { assertArrayEquals(Array(1, 2, 3), Array(1, 5, 6)) } must failWith("'2' is not equal to '5'; '3' is not equal to '6'")
    }
  }
}
