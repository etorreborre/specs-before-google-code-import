package org.specs.runner

import org.specs._
import org.specs.specification._

/**
 * This class can be used to search for specifications on a given path
 * and execute them.<br>
 * Usage: <code>object myFileRunner extends SpecsFileRunner(path, pattern)</code><br>
 * Where <code>path</code> is a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
 * and <code>pattern</code> is a regular expression which is supposed to match an object name extending a Specification
 * class named ".*Spec.*"
 *
 * The systems and examples can also be filtered by specifying patterns. These patterns can be passed as System properties,
 * by specifying -Dsus="regexp" or -Dexample="regexp".
 */
class SpecsFileRunner(path: String, val specFilterPattern: String, susFilter: String, exampleFilter: String) extends
  SpecsFinder(path, specFilterPattern, true) with Console {

  /** define the sus pattern on the SpecsFilter trait. */
  override def susFilterPattern = susFilter

  /** define the example pattern on the SpecsFilter trait. */
  override def exampleFilterPattern = exampleFilter

  /** short constructor with no filter for sus or examples. */
  def this(path: String, specFilterPattern: String) = this(path, specFilterPattern, ".*", ".*")

  /** short constructor with the path only. */
  def this(path: String) = this(path, ".*", ".*", ".*")
}
