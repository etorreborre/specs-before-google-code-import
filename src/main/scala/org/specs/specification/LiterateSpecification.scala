package org.specs.specification
import scala.xml._
import org.specs.util.Property
import org.specs.util.DataTables
import org.specs.util._
import org.specs.Sugar._
import org.specs.matcher._
import org.specs.runner._
/**
 * This trait is experimental. It is supposed to help writing some literate specifications
 * using the xml capabilities of Scala.
 * Several "toy" specifications have been written using this style:<ul>
 * <li>bizSpecificationSpec
 * <li>calculatorBizSpec
 * <li>xmlRunnerSpec
 * </ul>
 */
class LiterateSpecification extends Specification with ExpectableFactory with DataTables with Properties with Links {
  setSequential()

  def this(n: String) = { this(); name = n; description = n; this }

  /**
   * This method is used to silence the result of a call in an action. For example: <pre>
   * The timer should be stopped {timer.stop.shh}
   * </pre>. This will not output the result of the stop method
   */
  implicit def anyToShh(a: Any) = new Silenced

  /** allow empty sus to be reported. */
  override def filterEmptySus = false
  class Silenced {
    def shh = ()

    /** the pipe bar must be interpreted visually as a stop and the < sign as a pike. */
    def <| = shh
  }
  /** This silence function allows to silence calls with this style: shh { a call } */
  def shh(a: =>Any) = { a; () }

  /**
   * This method allows to embbed a DataTable in a literate specification and display the results of its execution
   */
  implicit def makeTable(s: String) = new TableExample(s)
  case class TableExample(desc: String) {
    def inTable(table: =>ExecutableDataTable) = {
      lazy val tableToExecute = table
      forExample(desc) in {
        tableToExecute.execute
      }
      desc + "\n" + tableToExecute.toHtml.toString
    }
    def inForm(form: =>org.specs.form.Form) = {
      lazy val formToExecute = form
      val description = if (desc.isEmpty) form.title else desc
      forExample(description) in {
          formToExecute.execute
          if (!formToExecute.isOk) throw new FailureException("The form '" +  formToExecute.title + "' failed")
      }
      description + "\n" + formToExecute.toHtml.toString
    }
  }

  /** create an anonymous example which will be skipped until it is implemented */
  def notImplemented = forExample in { skip ("not implemented yet")}
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
          example.exampleDescription = if (content.exists(_.label == "wiki")) WikiExampleDescription(node.first.text) else ExampleDescription(node.first.text)
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

  /**
   * Create an anonymous example with a function on a System,
   * giving it a number depending on the existing created examples/
   */
  def eg[S](function: S => Any): Unit = (forExample in function).shh

  /** embeddeds a test into a new example and silence the result */
  def eg(test: =>Any): Unit = (forExample in test).shh

  /** return a String containing the output messages from the console with a given padding such as a newline for instance */
  def consoleOutput(pad: String, messages: Seq[String]): String = { pad + consoleOutput(messages) }

  /** return a String containing the output messages from the console */
  def consoleOutput(messages: Seq[String]): String = messages.map("> " + _.toString).mkString("\n")

  def includeSus(susName: String) = "include " + susName + " not implemented yet"

  private var parentLinks = List[Specification]()
  def addParentLink(s: Specification): this.type = { parentLinks = s :: parentLinks; this }
  def hasParentLink(s: Specification) = parentLinks.contains(s)

  def linkTo(subSpec: LiterateSpecification with Html): String = linkTo(subSpec.description, subSpec)
  def linkTo(desc: String, subSpec: LiterateSpecification with Html): String = {
    if (!this.subSpecifications.contains(subSpec)) include(subSpec)
    subSpec.addParentLink(this)
    pathLink(desc, new java.io.File(subSpec.filePath(subSpec)).getAbsolutePath)
  }
}
/**
 * This trait provides functions which can be used to ease the use of wiki markup
 */
trait Wiki extends Properties with Links {
  implicit def toWikiString(a: Any) = new WikiString(a.toString)
  class WikiString(s: String) {
    def code = wikiCode(s)
    def >@ = wikiCode(s)
    def pre = wikiPre(s)
  }
  /**
   * This function can be used to format code in a wiki description.
   * Using this function avoid issues like quotes insides brackets ['something']
   * being displayed as question marks.
   */
  def wikiPre(stringToFormat: String) = <pre>stringToFormat</pre>
  def wikiCode(stringToFormat: String) = stringToFormat.replace("\r\n", "\n").
                                                        replace("\n\r", "\n").
          split("\n").map(htmlize(_)).mkString("==<code class=\"prettyprint\">", "</code>==\n==<code class=\"prettyprint\">", "</code>==")

  private def htmlize(s: String) = s.replace("<", "&lt;").replace(">", "&gt;")
  /**
   * Alias for wikiCode
   */
  def >@(stringToFormat: String) = wikiCode(stringToFormat)

  def linkTo(susName: String) = "link to " + susName + " not implemented yet"
  override def pathLink(desc: String, path: String) = {
    "\"" + desc + "\":file:///" + path
  }
}
trait Links {
  def pathLink(desc: String, path: String) = desc + " (See: " + path + ")"
}