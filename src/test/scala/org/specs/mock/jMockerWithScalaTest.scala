package org.specs.mock

import org.specs.mock.JMocker._
import org.specs.mock.JMocker.{expect => expecting}
import org.scalatest.Suite
 
/**
 * This sample class shows how to use ScalaTest with JMocker and how to avoid naming conflicts with the <code>expect</code> method
 */
class jMockerWithScalaTestSuite extends Suite {
  def testMockExpectations {
    val list: java.util.List[Object] = mock(classOf[java.util.List[Object]])
    expecting { one(list).size }
    list.size
    checkContext
  }
}
