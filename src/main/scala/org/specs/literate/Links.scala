package org.specs.literate

/**
 * This trait provides a default implementation for linking specifications together
 */
trait Links {
  def pathLink(desc: String, path: String) = desc + " (See: " + path + ")"
  def relativeLink(desc: String, path: String) = desc + " (See: " + path + ")"
}

