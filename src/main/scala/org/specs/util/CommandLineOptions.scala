package org.specs.util
import org.specs.io.Output

/**
 * This trait displays usage and options of a Main program on the the command line
 */
protected[specs] trait CommandLineOptions { this: Output =>
  val usage: String
  val optionsSummary: String
  val optionsDescription: String
  /** display all help options. */
  def displayHelp = {
    displayUsage
    displayOptions
    displayOptionsDescription
  }
  /** display the usage. */
  def displayUsage = println(usage)
  /** display the options summary. */
  def displayOptions = println(optionsSummary)
  /** display the options description. */
  def displayOptionsDescription = println(optionsDescription)
} 
