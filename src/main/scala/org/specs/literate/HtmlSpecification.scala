package org.specs
import org.specs.literate._
import org.specs.runner.Html
/**
 * LiterateSpecification with Html reporting
 */      
class HtmlSpecification extends LiterateSpecification with Html with Wiki {
  def this(n: String) = { 
    this()
    name = n
    description = n
    this 
  }
}
