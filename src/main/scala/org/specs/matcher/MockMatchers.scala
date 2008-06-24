package org.specs.matcher
import org.specs.mock._

/**
 * Matcher for mocks
 */
trait MockMatchers {
  /**
   * Matches if the expected messages are properly received
   */
  def beMet = new Matcher[Protocol](){
    def apply(protocol: => Protocol) = {
      val failures = protocol.failures
      (failures.length == 0, "all expectations are met", failures)
    }
  }
  /**
   * <code>any[Type]</code> can be used to match any parameter in a mock call<br>
   * Usage: <code>mock.callMethodWith(any[String])</code>
   */
  def any[T]: T = {
     null.asInstanceOf[T]
  }
}
