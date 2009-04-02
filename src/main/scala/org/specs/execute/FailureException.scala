package org.specs.execute

/** RuntimeException carrying a matcher ko message */
case class FailureException(var message: String) extends RuntimeException(message) {
  override def getMessage = message
}
