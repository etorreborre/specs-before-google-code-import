package org.specs.runner

import org.specs._
import org.specs.specification._
import java.util.regex.Pattern.compile

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
class SpecsFileRunner(path: String, specFilterPattern: String, susFilterPattern: String, exampleFilterPattern: String) extends 
  SpecsFinder(path, specFilterPattern, true) with Console {
  
  /** short constructor with no filter for sus or examples. */
  def this(path: String, specFilterPattern: String) = this(path, specFilterPattern, ".*", ".*")

  /** short constructor with the path only. */
  def this(path: String) = this(path, ".*", ".*", ".*")
  
  /** filtered specs to run. */
  override lazy val specs = filter(collectSpecs(asOneSpecification))

  /** pattern for the sus. */
  lazy val susFilter = compile(susPattern)

  /** pattern for the examples. */
  lazy val exampleFilter = compile(examplePattern)
  
  /** filter a list of specifications. */
  def filter(specifications: List[Specification]): List[Specification] = {
    specifications.flatMap(filter(_)).toList
  }

  /** 
   * filter a specification.
   * @return None if the resulting specification has no SUS or systems
   */
  def filter(specification: Specification): Option[Specification] = {
    specification.subSpecifications = specification.subSpecifications.flatMap(filter(_)).toList
    specification.systems = specification.systems.flatMap(filter(_)).toList
    if (specification.subSpecifications.isEmpty && specification.systems.isEmpty)
      None
    else
      Some(specification)
  }

  /** 
   * filter a SUS.
   * @return None if the resulting SUS has no examples
   */
  def filter(sus: Sus): Option[Sus] = {
    if (susFilter.matcher(sus.description).find) {
      filterExamples(sus) match {
        case s if s.examples.size > 0 => Some(s)
        case _ => None
      }
    } 
    else
      None
  }

  /** 
   * filter the examples of a SUS according to the regular expression.
   * @return None if the resulting SUS has no examples
   */
  def filterExamples(sus: Sus): Sus = {
    if (exampleFilterPattern == ".*") // to speed up the execution
      sus
    else {
      sus.examples_=(sus.examples.flatMap(filterExample(_)).toList)
      sus
    }
  }
  /** 
   * filter one example.
   * @return None if the example does not match the expected regular expression
   */
  def filterExample(example: Example): Option[Example] = {
    if (exampleFilter.matcher(example.description).find) 
      Some(example)
   else
      None
  }

  /** 
   * @return either the system property named "sus" or the class attribute
   */
  def susPattern: String = {
    System.getProperty("sus") match {
      case null => susFilterPattern
      case something => something
    }
  } 
  /** 
   * @return either the system property named "example" or the class attribute
   */
  def examplePattern: String = {
    System.getProperty("example") match {
      case null => exampleFilterPattern
      case something => something
    }
  } 
}
