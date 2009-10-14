package org.specs.literate
import org.specs.specification._
import org.specs.xml.NodeFunctions._
import scala.xml._

trait Documents extends BaseSpecification with Markdown with DocumentsFactory { 
  override implicit def specifyExample(desc: String) = {
    if (systemsList.isEmpty) {
      setCurrent(Some(specify(name)))
    }
    super.specifyExample(desc)
  }  
  override def systems = {
    if (!systemsList.isEmpty)
      currentDocument.map(d => systemsList(0).literateDescription = Some(d.toLiterateDesc))
    super.systems
  }
}

trait DocumentsFactory { this: BaseSpecification =>
  private[specs] var currentDocument: Option[Document] = None

  implicit def md(s: =>String) = new MarkdownText(s)
  implicit def ex(e: =>Example) = {
    new ExampleDoc(e)
  }

  class MarkdownText(text: =>String) extends Document {
    def m: this.type = this
    def toXhtml = <md>{((new Markdown {}).format(<m>{text}</m>) \ "p")(0) match {
      case <p>{a @ _*}</p> => a
      case other => other
    }
    }</md>
  }
  class LineBreak extends Document {
    def toXhtml = <br></br>
  }
  class ExampleDoc(e: Example) extends Document {
    def toXhtml = <ex>{e.description}</ex>
  }
  class DocSequence(documents: List[Document]) extends Document {
    def toXhtml = reduce(documents, { (d: Document) => d.toXhtml })
  }
  abstract class Document {
    if (currentDocument == None) currentDocument = Some(this)
    def \\(d: Document): Document = this append d
    def \\(s: =>String): Document = this append s.m
    def \(d: Document): Document = this plus d
    def \(s: =>String): Document = this plus s.m
    def append(d: Document): Document = {
      currentDocument = Some(new DocSequence(List(this, new LineBreak, d)))
      currentDocument.get
    }
    def plus(d: Document): Document = {
      currentDocument = Some(new DocSequence(List(this, d)))
      currentDocument.get
    }
    def toXhtml: NodeSeq
    def toLiterateDesc: LiterateDescription = LiterateDescription(<div>{ toXhtml }</div>)
  }
}

import org.specs.runner._
import scala.xml._
class DocumentSample extends LiterateSpecification with Documents with org.specs.runner.Html {

  """A document is a piece of text with _markup_ notation""" \\
  """It allows to compose a whole literate specification from small elements""" \\
  ("and examples too" in { 1 must_== 1 })

}

class DocumentSpec extends org.spex.Specification with DocumentsFactory {
  var sus = new Sus(this)
  val doc = new DocumentSample

  "A specification with documents" should {
    "have a default sus" in {
      doc.systems must have size 1
    }
    "display a String when using a HtmlRunner" in {
      docString must include("A document")
    }
    "display a formatted String with Markdown" in {
      docString must include("<em>markup</em>")
    }
    "have a \\\\ operator to append a document" in {
      docString must include("notation</md><br></br><md>It")
    }
    "have a \\ operator to inline an example" in {
      doc.asHtml(doc) must \\("ex")
    }
  }
  def docString = docBody toString
  def docBody = doc.asHtml(doc) \\ "body" 
    
  "A Markdown document" should {
    "have a toXhtml method displaying the document" in {
      "hello".m.toXhtml.toString must_== "<md>hello</md>" 
    }
    "have a \\ method to add another document" in {
      ("hello" \ "world") .toXhtml.toString must_== "<md>hello</md><md>world</md>" 
    }
    "have a \\\\ method to add another paragraph" in {
      ("hello" \\ "world") .toXhtml.toString must_== "<md>hello</md><br></br><md>world</md>" 
    }
  }
  "A document sequence" should {
    "have a toXhtml method calling the other documents toXhtml methods" in {
      ("hello" \ "beautiful" \ "world").toXhtml.toString must_== "<md>hello</md><md>beautiful</md><md>world</md>"
    }
  }
}


