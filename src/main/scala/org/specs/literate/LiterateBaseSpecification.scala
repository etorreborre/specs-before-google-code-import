package org.specs.literate
import org.specs.specification._
import org.specs.runner.DescriptionFormatter
import scala.xml._

trait LiterateBaseSpecification extends ExpectableFactory with BaseSpecification {
  private val descriptionFormatter = new DescriptionFormatter()
  implicit def toSus(e: => Elem): ToLiterateSus = new ToLiterateSus(e) 
  class ToLiterateSus(e: => Elem) {
    def isSus = toLiterateSus("") ->> e
  }
  implicit def toLiterateSusWithDesc(string: String) = new LiterateSus(specify(string))
  implicit def toLiterateSus(sus: Sus) = new LiterateSus(sus)

  /** This class acts as an extension of a Sus to provide a literate description of a sus as an xml specification */
  class LiterateSus(sus: Sus) {
    def ->>(e: => Elem)= {
      sus.verb = ""
      format(e)
    }
    /** specifies the system with a literate description and embedded expectations */
    def is(e: => Elem)= {
      sus.verb = "specifies"
      format(e)
    }
    /** associates every <ex> tag to an anonymous example */
    private def format(e: => Elem) = {
      try {      
        val content = e
        val anonymous = sus.examples.filter(_.description.matches("example \\d+"))
        val exNodes = content.\("ex")
        exNodes.theSeq.toList.zip(anonymous.toList).foreach { pair =>
          val (node, example) = pair
          example.exampleDescription = descriptionFormatter.makeExampleDescription(content, node)
          List("tag", "tags") foreach { tagName => addTag(node, example, tagName) }
        }
        sus.literateDescription = Some(content)
      } catch {
        case t => forExample("The system could not be evaluated").addError(t)
      }
    }
    private def addTag(node: Node, example: Example, tagName: String) = {
      node.attribute(tagName) match {
        case None => ()
        case Some(a) => a.toString.split(",").foreach(t => example.addTag(t.trim))
      }
    }
  }
}
