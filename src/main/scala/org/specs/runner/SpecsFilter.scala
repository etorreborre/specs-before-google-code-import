package org.specs.runner

import org.specs.specification._
import java.util.regex.Pattern.compile

trait SpecsFilter extends SpecsHolder {

  /**default regexp for filtering sus. */
  def susFilterPattern = ".*"

  /**default regexp for filtering examples. */
  def exampleFilterPattern = ".*"

  /**filtered specs to run. */
  lazy val filteredSpecs = filter(specs)

  /**pattern for the sus. */
  lazy val susFilter = compile(susPattern)

  /**pattern for the examples. */
  lazy val exampleFilter = compile(examplePattern)

  /**filter a list of specifications. */
  def filter(specifications: Seq[Specification]): List[Specification] = {
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