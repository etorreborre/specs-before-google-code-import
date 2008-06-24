package org.specs.matcher;
import MatcherUtils._
import java.util.regex._
/**
 * The <code>StringMatchers</code> trait provides matchers which are applicable to String objects
 */
trait StringMatchers {
  
  /**
   * Matches if (a.equalsIgnoreCase(b))
   */   
  def equalIgnoreCase[T <: String](a: T) = new Matcher[T](){ 
    def apply(v: => T) = {val b = v; (a != null && b != null && a.equalsIgnoreCase(b), q(b) + " is equal ignoring case to " + q(a), q(b) + " is not equal ignoring case to " + q(a))} 
  }

  /**
   * Matches if !(a.equalsIgnoreCase(b))
   */   
  def notEqualIgnoreCase[T <: String](a: T) = equalIgnoreCase(a).not 

  /**
   * Matches if (a.trim == b.trim)
   */   
  def equalIgnoreSpace[T <: String](a: T) = new Matcher[T](){ 
    def apply(v: => T) = {val b = v; (a != null && b != null && a.trim == b.trim, q(b) + " is equal ignoring space to " + q(a), q(b) + " is not equal ignoring space to " + q(a))} 
  }

  /**
   * Matches if !(a.equalsIgnoreSpace(b))
   */   
  def notEqualIgnoreSpace[T <: String](a: T) = equalIgnoreSpace(a).not 

  /**
   * Matches if (b.indexOf(a) >= 0)
   */   
  def include[T <: String](a: String) = new Matcher[T](){ 
    def apply(v: => T) = {val b = v; (a != null && b != null && b.indexOf(a) >= 0, q(b) + " includes " + q(a), q(b) + " doesn't include " + q(a))} 
  }

  /**
   * Matches if !(b.indexOf(a) >= 0)
   */   
  def notInclude[T <: String](a: T) = include[T](a).not 

  /**
   * Matches if b matches the regular expression a
   */   
  def beMatching[T <: String](a: T) = new Matcher[T](){
    def apply(v: => T) = {val b = v; (matches(a)(b), q(b) + " matches " + q(a), q(b) + " doesn't match " + q(a))}
  }

  /**
   * Matches if b doesn't match the regular expression a
   */   
  def notBeMatching(a: String) = beMatching[String](a).not

  /**
   * Matches if b.startsWith(a)
   */   
  def startWith[T <: String](a: T) = new Matcher[T](){ 
     def apply(v: => T) = {val b = v; (b!= null && a!= null && b.startsWith(a), q(b) + " starts with " + q(a), q(b) + " doesn't start with " + q(a))} 
  }
  /**
   * Matches if !b.startsWith(a)
   */   
  def notStartWith(a: String) = startWith[String](a).not

  /**
   * Matches if b.endsWith(a)
   */   
  def endWith[T <: String](a: T) = new Matcher[T](){ 
     def apply(v: => T) = {val b = v; (a != null && b != null && b.endsWith(a), q(b) + " ends with " + q(a), q(b) + " doesn't end with " + q(a))} 
  }

  /**
   * Matches if !b.endsWith(a)
   */   
  def notEndWith(a: String) = endWith[String](a).not

  /**
   * Matches if the regexp a is found inside b
   */   
  def find[T <: String](a: T) = new FindMatcher(a)

  /**
   * Matcher to find if the regexp a is found inside b. 
   * This matcher can be specialized to a FindMatcherWithGroups which will also check the found groups
   */   
  class FindMatcher[T <: String](a: T) extends Matcher[T] {
    def found(b: T) = {
      val matcher = Pattern.compile(a).matcher(b)
      matcher.find
    }
    def withGroup(group: String) = new FindMatcherWithGroups(a, group)
    def withGroups(groups: String*) = new FindMatcherWithGroups(a, groups:_*)
    def apply(v: => T) = {val b = v; (a != null && b != null && found(b), q(a) + " is found in " + q(b), q(a) + " isn't found in " + q(b))} 
  }

  /**
   * Matcher to find if the regexp a is found inside b. 
   * This matcher checks if the found groups are really the ones expected
   */   
  class FindMatcherWithGroups[T <: String](a: T, groups: String*) extends Matcher[T] {
    def found(b: T) = {
      val matcher = Pattern.compile(a).matcher(b)
      val groupsFound = new scala.collection.mutable.ListBuffer[String]()
      while (matcher.find) { groupsFound += matcher.group(1) }
      groupsFound.toList
    }
    def apply(v: => T) = {
      val b = v
      val groupsFound = found(b)
      val withGroups = if (groups.size > 1) " with groups " else " with group "
      def foundText = {
        if (groupsFound.isEmpty) 
          ". Found nothing" 
        else 
           ". Found: " + q(groupsFound.mkString(", "))
      }
      val groupsToFind = if (groups == null) Nil else groups.toList
      (a != null && b != null && groupsFound == groupsToFind, 
       q(a) + " is found in " + q(b) + withGroups + q(groupsToFind.mkString(", ")), 
       q(a) + " isn't found in " + q(b) + withGroups + q(groupsToFind.mkString(", ")) + foundText 
       )
    } 
  }
}
