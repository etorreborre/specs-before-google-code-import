package org.specs.util
import org.specs._

class lazyParamSpec extends SpecificationWithJUnit with LazyParameters {
  def method[T](values: LazyParameter[T]*) = {
    addMessage("method")
	values.toStream.foreach(_())
  }
  "a function" can {
    "be called with lazy var arguments" in {
      method(addMessage("1"), addMessage("2"))
      messages must_== List("method", "1", "2")
    }
  }
  var messages: List[String] = Nil
  def addMessage(m: String) = {
    messages = messages ::: List(m)
	m
  }
}
