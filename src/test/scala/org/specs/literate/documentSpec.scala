package org.specs.literate
import org.specs.specification._
import org.specs.xml.NodeFunctions._
import scala.xml._

trait Documents extends DocumentsFactory with Markdown with SpecificationSystems { this: BaseSpecification =>
  val sus = specify(name)
  override def systems = {
    currentDocument.map(d => sus.literateDescription = Some(d.toLiterateDesc))
    super.systems
  }
}

trait DocumentsFactory {
  private[specs] var currentDocument: Option[Document] = None

  implicit def MD(s: =>String) = new MarkdownText(s)

  class MarkdownText(text: =>String) extends Document {
    def m: this.type = this
    def toXhtml = (new Markdown {}).format(<m>{text}</m>) \ "p"
  }
  class Composite(documents: List[Document]) extends Document {
    def toXhtml = reduce(documents, { (d: Document) => d.toXhtml })
  }
  abstract class Document {
    if (currentDocument == None) currentDocument = Some(this)
    def \(d: Document): Document = this append d
    def \(s: =>String): Document = this append s.m
    def append(d: Document): Document = {
      currentDocument = Some(new Composite(this :: List(d)))
      currentDocument.get
    }
    def toXhtml: NodeSeq
    def toLiterateDesc: LiterateDescription = LiterateDescription(<div>{ toXhtml }</div>)
  }
}

import org.specs.runner._
import scala.xml._
class DocumentSample extends LiterateSpecification with Documents with org.specs.runner.Html {

  """A document is a piece of text with _markup_ notation""" \
  """It allows to compose a whole literate specification from small elements"""

}

class DocumentSpec extends org.spex.Specification with DocumentsFactory {
  var sus = new Sus(this)
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
    "have a \\ operator to append documents" in {
      docBody must include("notation</p><p>It")
    }
  }
  def docBody = doc.asHtml(doc) \\("p") toString
    
  "A Markdown document" should {
    "have a toXhtml method displaying the document" in {
      "hello".m.toXhtml.toString must_== "<p>hello</p>" 
    }
  }
  "A composite document" should {
    "have a toXhtml method calling the other documents toXhtml methods" in {
      ("hello" \ "beautiful" \ "world").toXhtml.toString must_== "<p>hello</p><p>beautiful</p><p>world</p>"
    }
  }
}


