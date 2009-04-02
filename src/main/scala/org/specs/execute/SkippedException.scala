package org.specs.execute

/** RuntimeException carrying a matcher skip message */
case class SkippedException(var message: String) extends RuntimeException(message) {
  override def getMessage = message
}
