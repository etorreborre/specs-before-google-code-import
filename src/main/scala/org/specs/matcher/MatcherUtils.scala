package org.specs.matcher
import java.util.regex._

/**
 * This object provides utility functions for matchers
 */
object MatcherUtils {

  /**
   * @return true if b is matching the regexp a
   */
  def matches[T <: String](a: String)(b: T) = a != null && b != null && Pattern.compile(a).matcher(b).find 

  /**
   * @return true if a string s can be parsed to an integer
   */
  def isInteger(s: String): Boolean = {try {s.toInt} catch {case _ => return false}; true}

  /**
   * @return an object.toString() between quotes (used in messages creation)
   */
  def q(a: Any)  = if (a == null) "'null'" else "'" + a.toString + "'"
  
  /**
   * @return an s if i is > 1
   */
  def plural(word: String, i: Int) = if (i > 1) (word + "s") else word
}
