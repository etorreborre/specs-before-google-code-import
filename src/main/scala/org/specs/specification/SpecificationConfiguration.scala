package org.specs.specification

import org.specs.util.Configuration

/**
 * This trait defines some optional behaviour for a specification such as executing examples in a copy of the specification
 * to be isolated from any other example modifying local variables.
 */
trait SpecificationConfiguration {
  /** get the configuration state */
  private[specification] var oneSpecInstancePerExample = Configuration.config.oneSpecInstancePerExample
  /** 
   * use this method to use the same specification object to execute Examples, effectively sharing
   * variables between them. 
   */
  def shareVariables() = oneSpecInstancePerExample = false
  /** 
   * use this method *not* to use the same specification object to execute Examples, effectively *not* sharing
   * variables between them. 
   */
  def dontShareVariables() = oneSpecInstancePerExample = true
}
