package org.specs.execute

/**
 * This trait can be added to anything that can be commented.
 */
trait Commentable {
  private var commented = false
  def comment(): this.type = { commented = true; this }
  def uncomment(): this.type = { commented = false; this }
  def isCommented = commented
}
