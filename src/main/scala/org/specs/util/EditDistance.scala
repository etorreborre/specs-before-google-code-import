package org.specs.util
import org.specs.util.ExtendedString._

/** 
 * The EditDistance object provides methods to compute and display the shortest distance between 2 strings.<p>
 * Usage:<pre>
 * showDistance("kitten", "sitting") // returns ("(k)itt(e)n", "(s)itt(i)n(g)")
 * 
 * // with different separators
 * showDistance("kitten", "sitting", "[]") // returns ("[k]itt[e]n", "[s]itt[i]n[g]")
 * </pre>
 */
object EditDistance extends EditDistance

/** 
 * The EditDistance trait provides methods to compute and display the shortest distance between 2 strings.<p>
 * Usage:<pre>
 * showDistance("kitten", "sitting") // returns ("(k)itt(e)n", "(s)itt(i)n(g)")
 * 
 * // with different separators
 * showDistance("kitten", "sitting", "[]") // returns ("[k]itt[e]n", "[s]itt[i]n[g]")
 * </pre>
 */
trait EditDistance {
  /**
   * Class encapsulating the functions related to the edit distance of 2 strings
   */
  case class EditMatrix(s1: String, s2: String) {
    /* matrix containing the edit distance for any prefix of s1 and s2: matrix(i)(j) = edit distance(s1[0..i], s[0..j])*/
    val matrix = new Array[Array[Int]](s1.length + 1, s2.length + 1)

    /* initializing the matrix */
    for (i <- 0 to s1.length;
         j <- 0 to s2.length) {
      if (i == 0) matrix(i)(j) = j // j insertions
      else if (j == 0) matrix(i)(j) = i  // i suppressions
      else matrix(i)(j) = min(matrix(i - 1)(j) + 1, // suppression
                              matrix(i - 1)(j - 1) + (if (s1(i - 1) == s2(j - 1)) 0 else 1), // substitution
                              matrix(i)(j - 1) + 1) // insertion
    
    }
    /** @return the edit distance between 2 strings */
    def distance = matrix(s1.length)(s2.length)

    /** prints the edit matrix of 2 strings */
    def print = { 
      for (i <- 0 to s1.length) {
        def row = for (j <- 0 to s2.length) yield matrix(i)(j)
        println(row.mkString("|"))
      }
      this
    }

    /** @return a (String, String) displaying the differences between each input strings. The used separators are parenthesis: '(' and ')'*/
    def showDistance: (String, String) = showDistance("()") 

    /**
     * @param sep separators used to hightlight differences. If sep is empty, then no separator is used. If sep contains 
     * one character, it is taken as the unique separator. If sep contains 2 or more characters, half of them are taken for the opening separator and
     * the rest for the closing separator.
     * 
     * @return a (String, String) displaying the differences between each input strings. The used separators are specified by the caller.<p>
     */
    def showDistance(sep: String) = {
      val (firstSeparator, secondSeparator) = separators(sep)
	  def modify(s: String, c: Char): String = modifyString(s, c.toString)
	  def modifyString(s: String, mod: String): String = (firstSeparator + mod + secondSeparator + s).removeAll(secondSeparator + firstSeparator)
      def findOperations(dist: Int, i: Int, j:Int, s1mod: String, s2mod: String): (String, String) = {
        if (i == 0 && j == 0) {
  	      ("", "") 
        }
        else if (i == 1 && j == 1) {
  	      if (dist == 0) (s1(0) + s1mod, s2(0) + s2mod)
          else (modify(s1mod, s1(0)), modify(s2mod, s2(0))) 
        }
        else if (j < 1) (modifyString(s1mod, s1.slice(0, i)), s2mod)
        else if (i < 1) (s1mod, modifyString(s2mod, s2.slice(0, j)))
        else {
	      val (suppr, subst, ins) = (matrix(i - 1)(j), matrix(i - 1)(j - 1), matrix(i)(j - 1))   
	      if (suppr < subst) 
	        findOperations(suppr, i - 1, j, modify(s1mod, s1(i - 1)), s2mod)
	      else if (ins < subst)
	        findOperations(ins, i, j - 1, s1mod, modify(s2mod, s2(j - 1)))
	      else if (subst < dist)
	        findOperations(subst, i - 1, j - 1, modify(s1mod, s1(i - 1)), modify(s2mod, s2(j - 1)))
	      else
	        findOperations(subst, i - 1, j - 1, s1(i - 1) + s1mod, s2(j - 1) + s2mod)
	    }
      }
      findOperations(distance, s1.length, s2.length, "", "")
    }
    def min(suppr: Int, subst: Int, ins: =>Int) = {
      if(suppr < subst) suppr
      else if (ins < subst) ins
      else subst
    }
  }
  /** @return the edit distance between 2 strings = the minimum number of insertions/suppressions/substitutions to pass from one string to the other */
  def editDistance(s1: String, s2: String): Int = EditMatrix(s1, s2).distance

  /** prints on the console the edit matrix for 2 strings */
  def showMatrix(s1: String, s2: String) = EditMatrix(s1, s2).print

  /** @return a (String, String) displaying the differences between each input strings. The used separators are parenthesis: '(' and ')'*/
  def showDistance(s1: String, s2: String) = EditMatrix(s1, s2).showDistance
  /**
   * @param sep separators used to hightlight differences. If sep is empty, then no separator is used. If sep contains 
   * one character, it is taken as the unique separator. If sep contains 2 or more characters, the first half of the characters are taken as
   * opening separator and the second half as closing separator.
   * 
   * @return a (String, String) displaying the differences between each input strings. The used separators are specified by the caller.<p>
   */
  def showDistance(s1: String, s2: String, sep: String) = EditMatrix(s1, s2).showDistance(sep)

  private def separators(s: String) = (firstSeparator(s), secondSeparator(s))
  private def firstSeparator(s: String) = if (s.isEmpty) "" else s.substring(0, s.size / 2 + s.size % 2)
  private def secondSeparator(s: String) = if (s.size < 2) firstSeparator(s) else s.substring(s.size / 2 + s.size % 2, s.size)
}