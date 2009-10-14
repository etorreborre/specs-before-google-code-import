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
  def br = new LineBreak
  class LineBreak extends Document {
    def toXhtml = <br></br>
  }
  class ExampleDoc(e: Example) extends Document {
    def toXhtml = <ex>{e.description}</ex>
  }
  class DocSequence(documents: List[Document]) extends Document {
    def toXhtml = reduce(documents, { (d: Document) => d.toXhtml })
  }
  class Decorator(document: Document, decorator: (NodeSeq => NodeSeq)) extends Document {
    def toXhtml = decorator(document.toXhtml)
  }
  class Paragraph(document: Document) extends Decorator(document, (d: NodeSeq) => <p>{d}</p>)
  
  abstract class Document {
    if (currentDocument == None) currentDocument = Some(this)
    def \\(d: Document): Document = parAddPar(d)
    def \\(s: =>String): Document = \\(s.m)
    def \:(d: Document): Document = add(new Paragraph(d), this)
    def :\(d: Document): Document = addPar(d)
    def \(d: Document): Document = plus(d)
    def \(s: =>String): Document = \(s.m)
    
    def parAddPar(d: Document): Document = add(new Paragraph(this), new Paragraph(d))
    def addPar(d: Document): Document = add(this, new Paragraph(d))
    def parAdd(d: Document): Document = add(new Paragraph(this), d)
    def add(d1: Document, d2: Document) = {
      currentDocument = Some(new DocSequence(List(d1, d2)))
      currentDocument.get
    }
    def plus(d: Document): Document = add(this, d)
    def toXhtml: NodeSeq
    def toLiterateDesc: LiterateDescription = LiterateDescription(<div>{ toXhtml }</div>)
  }
}

import org.specs.runner._
import scala.xml._
class DocumentSample extends LiterateSpecification with Documents with org.specs.runner.Html {

  """A document is a piece of text with _markup_ notation""" \\
  """It allows to compose a whole literate specification from small elements""" \
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
      docString must include("notation</md></p><p><md>It")
    }
    "have a \\ operator to inline an example" in {
      doc.asHtml(doc) must \\("ex")
    }
    "allow break returns between docs" in {
      ("hello" \: br :\  "world" toXhtml).toString must include("hello</md></p><br></br><p><md>world")
    }
  }
  def docString = docBody toString
  def docBody = doc.asHtml(doc) \\ "body" 
    
  "A Document" should {
    "have a \\ method to add another document" in {
      ("hello" \ "world" toXhtml).toString must_== "<md>hello</md><md>world</md>" 
    }
    "have a \\\\ method to create 2 paragraphs and add them" in {
      ("hello" \\ "world" toXhtml).toString must_== "<p><md>hello</md></p><p><md>world</md></p>" 
    }
    "have a :\\ method to create a paragraph for the second doc and add it to the first" in {
      ("hello".m :\ "world" toXhtml).toString must_== "<md>hello</md><p><md>world</md></p>" 
    }
    "have a \\: method to create a paragraph for the first doc and add it to the second" in {
      ("hello" \: "world" toXhtml).toString must_== "<p><md>hello</md></p><md>world</md>" 
    }
  }
  "A Markdown document" should {
    "have a toXhtml method displaying the document" in {
      "hello".m.toXhtml.toString must_== "<md>hello</md>" 
    }
  }
  "A document sequence" should {
    "have a toXhtml method calling the other documents toXhtml methods" in {
      ("hello" \ "beautiful" \ "world").toXhtml.toString must_== "<md>hello</md><md>beautiful</md><md>world</md>"
    }
  }
}


