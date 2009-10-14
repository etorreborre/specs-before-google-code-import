package org.specs.literate
import org.specs.specification._
trait Documents extends LiterateSpecification with Markdown with SpecificationSystems {
  val sus = specify(name)
  private var doc: Option[Document] = None
  override def systems = {
    doc.map(d => sus.literateDescription = Some(d.toLiterateDesc))
    super.systems
  }
  implicit def MD(s: =>String) = new MarkdownText(s)
  class MarkdownText(text: =>String) extends Document {
    def m: this.type = this
    def toLiterateDesc = LiterateDescription((new Markdown {}).format(<m>{text}</m>))
  }

  abstract class Document {
    if (doc == None) doc = Some(this)
    def \(d: Document): this.type = this
    def \(s: String): this.type = this \ s.m
    def toLiterateDesc: LiterateDescription
  }
}

import org.specs.runner._
import scala.xml._
class DocumentSample extends LiterateSpecification with Documents with org.specs.runner.Html {

  """A document is a piece of text with _markup_ notation""" \
  """It allows to compose a whole literate specification from small elements"""

}

class DocumentSpec extends org.spex.Specification {
  val doc = new DocumentSample

  "A specification with documents" should {
    "have a default sus" in {
      doc.systems must have size 1
    }
    "display a String when using a HtmlRunner" in {
      docBody must include("A document")
    }
    "display a formatted String with Markdown" in {
      docBody must include("<em>markup</em>")
    }
  }
  def docBody = doc.asHtml(doc) \\("p") toString
}


