package org.specs.literate
import org.specs.specification._
import org.specs.runner.Html
/**
 * This trait allows to add links to other specifications inside a literate specification.
 * The link will be displayed as a Html link
 * 
 * The "parent/child" relationship is kept in that trait to allow the Html runner
 * to be reported when reporting the parent.
 */
trait LiterateSpecificationLinks extends LinkedSpecification with Links { this: Specification => 
  def linkTo(subSpec: Specification with Html): String = linkTo(subSpec.description, subSpec)
  def linkTo(desc: String, subSpec: Specification with Html): String = {
    super.linkTo(desc, subSpec)
    // execute the subSpec
    subSpec.failures
    relativeLink(desc, subSpec.fileName(subSpec))
  }
}
