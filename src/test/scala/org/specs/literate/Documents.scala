package org.specs.literate
import org.specs.specification.{ BaseSpecification, Example, Sus, LiterateDescription }
import org.specs.xml.NodeFunctions._
import org.specs.form.ToXhtml
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

  implicit def toText(s: String) = new TextDoc(s.stripMargin)
  implicit def md(s: =>String) = new MarkdownString(s.stripMargin)
  implicit def ex(e: =>Example) = {
    new ExampleDoc(e)
  }

  case class TextDoc(text: String) extends Doc {
    def txt = this
    def toXhtml_! : NodeSeq = Text(text)
    override def toXhtml: NodeSeq = Text(text)
    def toText = text
  }
  class MarkdownString(val text: String) {
    def md: this.type = this
  }
  implicit def markdownDoc(s: MarkdownString) = new MarkdownText(s.text)
  private def markdownDecorator: NodeSeq => NodeSeq = markdownFormat((_:NodeSeq))
  private def markdownFormat(t: String): NodeSeq = markdownFormat(<m>{t}</m>)
  private def markdownFormat(n: NodeSeq): NodeSeq = markdownFormat(<m>{n}</m>)
  private def markdownFormat(n: Elem): NodeSeq = {
    val formatted = (new Markdown {}).format(n) \ "p"
    formatted match {
      case <p>{a @ _*}</p> => a
      case other => other
    }
  } 
  case class MarkdownText(text: String) extends Doc {
    override def paragraph = MarkdownText(text + "\n")
    def toXhtml_! = <md>{markdownFormat(text)}</md>
    override def toXhtml = <md>{markdownFormat(text)}</md>
    def toText = text
  }
  def p = br \ br
  def br = new LineBreak
  class LineBreak extends Doc {
    def toXhtml_! = <br></br>
    override def toXhtml = <br></br>
    def toText = "\n"
  }
  class ExampleDoc(e: Example) extends Doc {
    lazy val ex = e
    def toXhtml_! = <ex class={ex.statusClass}>{ex.description}</ex>
    override def toXhtml = <ex class="info">{ex.description}</ex>
    override def toText_! = ex.statusAsText + " " + ex.description
    def toText = ex.description
  }
  case class DocSequence(documents: List[Doc]) extends Doc {
    def toXhtml_! = reduce(documents, { (d: Doc) => d.toXhtml_! })
    override def toXhtml = reduce(documents, { (d: Doc) => d.toXhtml })
    def toText = documents.map(_.toText).mkString("")
  }
  case class Decorator(document: Doc, d: (NodeSeq => NodeSeq)) extends Doc {
    def toXhtml_! = d(document.toXhtml_!)
    override def toXhtml = d(document.toXhtml)
    def toText = toXhtml.text
  }
  class Paragraph(document: Doc) extends Doc 
  {
    def toXhtml_! = <p>{ document.toXhtml_! }</p> 
    override def toXhtml = <p>{ document.toXhtml }</p> 
    def toText = document.toText + "\n\n"
  }
  
  abstract class Doc extends Document {
    
    if (systemsList.isEmpty) {
      setCurrent(Some(specify(outer.name)))
      currentDocument = Some(this)
    }
    setCurrent(Some(systemsList.last))
    
    def \\(s: String): Doc = \\(new TextDoc(s))
    def ^^(d: Doc): Doc = \\(d)
    def \\(d: Doc): Doc = parAddPar(d)
    def \-(s: String): Doc = parAdd(new TextDoc(s))
    def \-(d: Doc): Doc = parAdd(d)
    def -\(s: String): Doc = addPar(new TextDoc(s))
    def -\(d: Doc): Doc = addPar(d)
    def \(s: String): Doc = \(new TextDoc(s))
    def \(d: Doc): Doc = plus(d)
    
    def parAddPar(d: Doc): Doc = add(this, br).add(br).add(d)
    def addPar(d: Doc): Doc = add(this, br).add(d)
    def parAdd(d: Doc): Doc = add(this, br).add(d)
    def add(d2: Doc): Doc = add(this, d2)
    def add(d1: Doc, d2: Doc): Doc = {
      val newDocument = (d1, d2) match {
        case (DocSequence(l), DocSequence(l2)) => new DocSequence(l ::: l2)
        case (l1, DocSequence(l)) => new DocSequence(l1 :: l)
        case (DocSequence(l), l2) => new DocSequence(l ::: List(l2))
        case (a, b) => new DocSequence(List(d1, d2))
      }
      currentDocument = Some(newDocument)
      currentDocument.get
    }
    def paragraph: Doc = new Paragraph(this)
    def plus(d: Doc): Doc = add(this, d)
    def toLiterateDesc: LiterateDescription = LiterateDescription(<div>{ toXhtml }</div>)
    
  }
  
  implicit def stringToSusList(desc: String) = new SusList(desc)
  class SusList(desc: String)
  {
    def ul(a: =>Any) = {
      val sus = desc.should(a)
      sus.verb = ""
      susToDocuments(sus)
    }
  }
  implicit def susToDocuments(sus: Sus) = new SusDoc(sus)
  class SusDoc(sus: Sus) extends Doc {
    lazy val s = sus
    def documents: List[Document] = s.examples.map(new ExampleDoc(_))
    override def toXhtml = 
      <sus>
        <t>{sus.description + " " + sus.verb}</t>
        <ul>
          { reduce(documents, { (d: Document) => <li>{ d.toXhtml }</li> }) }
        </ul>
      </sus> 
      
    def toXhtml_! = reduce(documents, { (d: Document) => d.toXhtml_! })
    def toText = {
      ((sus.description + " " + sus.verb) :: documents.map("  - " + _.toText)).mkString("\n")
    }
    override def toText_! : String = {
      ((sus.statusAsText + " " + sus.description + " " + sus.verb) :: documents.map("  " + _.toText_!)).mkString("\n")
    }
  }
}
trait Document extends ToXhtml {
  def toXhtml: NodeSeq
  def toXhtml_! : NodeSeq
  def toText: String
  def toText_! : String = toText
}
trait MarkdownDocs extends FormattedDocs
trait FormattedDocs

class DocumentSpecification extends LiterateSpecification with Documents with org.specs.runner.Html {
  override def reportSpec(spec: Specification, padding: String): this.type = {
    timer.start
    println(padding + "Specification \"" + spec.name + "\"")
    report(spec.subSpecifications, padding + "  ")
    currentDocument.map((d: Doc) => println(d.toText))
    timer.stop

    // if we want final statistics only, we check the padding to know if we're
    // reporting the first specification. An empty padding means this is the first spec.
    val isFirstSpecification = padding.isEmpty
    if (statistics() && (!finalStatisticsOnly() ||
                         finalStatisticsOnly() && isFirstSpecification))  {
      println(padding + "Total for specification \"" + spec.name + "\":")
      printStats(stats(spec), padding)
    }
    this
  }

  override def printSus(sus: Sus, padding: String) = {
    var susDescription = if (sus.isAnonymous) "" else sus.description + " " + sus.verb

    if (!sus.literateDesc.isEmpty) 
      println(padding + sus.literateDescText)
    else
      println(padding + susDescription)
    timer.start
    if (!planOnly() && sus.hasOwnFailureOrErrors)
      reportExample(sus, padding)
    reportExamples(sus.examples, padding)
    timer.stop
    println("")
  }

}