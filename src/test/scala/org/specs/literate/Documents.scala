package org.specs.literate
import org.specs.specification.{ BaseSpecification, Example, Sus, LiterateDescription }
import org.specs.xml.NodeFunctions._
import scala.xml._

trait Documents extends BaseSpecification with Markdown with DocumentsFactory { 
  override implicit def specifyExample(desc: String) = {
    if (systemsList.isEmpty) {
      setCurrent(Some(specify(name)))
    }
    super.specifyExample(desc)
  }  
}

trait DocumentsFactory { outer: BaseSpecification =>
  private[specs] var currentDocument: Option[Doc] = None

  implicit def toText(s: String) = new TextDoc(s)
  implicit def md(s: =>String) = new MarkdownString(s)
  implicit def ex(e: Example) = {
    new ExampleDoc(e)
  }

  class TextDoc(text: String) extends Doc {
    def txt = this
    def toXhtml: NodeSeq = Text(text)
    def toText = text
  }
  class MarkdownString(val text: String) {
    def md: this.type = this
  }
  implicit def markdownDoc(s: MarkdownString) = new MarkdownText(s.text)
  class MarkdownText(text: =>String) extends Doc {
    def toXhtml = <md>{((new Markdown {}).format(<m>{text}</m>) \ "p")(0) match {
      case <p>{a @ _*}</p> => a
      case other => other
    }
    }</md>
    def toText = text
  }
  def br = new LineBreak
  class LineBreak extends Doc {
    def toXhtml = <br></br>
    def toText = "\n"
  }
  class ExampleDoc(e: Example) extends Doc {
    lazy val ex = e
    def toXhtml = <ex>{ex.description}</ex>
    def toText = ex.description
  }
  class DocSequence(documents: List[Doc]) extends Doc {
    def toXhtml = reduce(documents, { (d: Doc) => d.toXhtml })
    def toText = documents.map(_.toText).mkString("")
  }
  class Decorator(document: Doc, decorator: (NodeSeq => NodeSeq)) extends Doc {
    def toXhtml = decorator(document.toXhtml)
    def toText = toXhtml.toString
  }
  class Paragraph(document: Doc) extends Decorator(document, (d: NodeSeq) => <p>{d}</p>)
  
  abstract class Doc extends Document {
    if (systemsList.isEmpty) {
      setCurrent(Some(specify(outer.name)))
      currentDocument = Some(this)
    } else
      currentDocument.map(d => systemsList(0).literateDescription = Some(d.toLiterateDesc))
    setCurrent(Some(systemsList.last))
    
    def \\(s: String): Doc = \\(new TextDoc(s))
    def \\(d: Doc): Doc = parAddPar(d)
    def \-(s: String): Doc = parAdd(new TextDoc(s))
    def \-(d: Doc): Doc = parAdd(d)
    def -\(s: String): Doc = addPar(new TextDoc(s))
    def -\(d: Doc): Doc = addPar(d)
    def \(s: String): Doc = \(new TextDoc(s))
    def \(d: Doc): Doc = plus(d)
    
    def parAddPar(d: Doc): Doc = add(new Paragraph(this), new Paragraph(d))
    def addPar(d: Doc): Doc = add(this, new Paragraph(d))
    def parAdd(d: Doc): Doc = add(new Paragraph(this), d)
    def add(d1: Doc, d2: Doc) = {
      currentDocument = Some(new DocSequence(List(d1, d2)))
      currentDocument.get
    }
    def plus(d: Doc): Doc = add(this, d)
    def toXhtml: NodeSeq
    def toLiterateDesc: LiterateDescription = LiterateDescription(<div>{ toXhtml }</div>)
  }
  
  implicit def susToDocuments(sus: Sus) = new SusDoc(sus)
  class SusDoc(sus: Sus) extends Doc {
    lazy val s = sus
    def documents: List[Document] = s.examples.map(new ExampleDoc(_))
    def toXhtml = reduce(documents, { (d: Document) => d.toXhtml })
    def toText = documents.map(_.toText).mkString("\n")
  }
}
trait Document {
  def toXhtml: NodeSeq
  def toText: String
}