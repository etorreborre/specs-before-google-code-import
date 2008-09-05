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
class LiterateSpecification extends Specification with AssertFactory with DataTables {
  setSequential
  def this(n: String) = { this(); name = n; description = n; this }
  
  /**
   * This method is used to silence the result of a call in an action. For example: <pre>
   * The timer should be stopped {timer.stop.shh}
   * </pre>. This will not output the result of the stop method
   */
  implicit def anyToShh(a: Any) = new Silenced
  
  class Silenced {
    def shh = ""
    
    /** the pipe bar must be interpreted visually as a stop and the < sign as a pike. */
    def <| = shh
  }
  /** This silence function allows to silence calls with this style: shh { a call } */
  def shh(a: =>Any) = { a; "" }

  /**
   * This method is used setup a property value, in order to avoid repeting a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = new AsProperty(a)
  class AsProperty[T](a: T) { 
    def as(p: Property[T]) = {p() = a; p.toString }
    def apply(p: Property[T]) = {p() = a; p.toString }
    def apply(f: T => Any)= {f(a); a.toString }
    def as(f: T => Any)= {f(a); a.toString }
  }
  
  /**
   * This method allows to embbed a DataTable in a literate specification and display the results of its execution
   */
  implicit def makeTable(s: String) = new TableExample(s)
  case class TableExample(desc: String) {
    def inTable(table: ExecutableDataTable) = {
      forExample(desc) in {
        table.execute
        table.results
      }
      desc + "\n" + table.toString
    }
  }    
  
  /** create an anonymous example which will be skipped until it is implemented */
  def notImplemented = forExample in { skip ("not implemented yet")}
  implicit def toSus(e: => Elem) = new Object { def isSus = toLiterateSus("") ->> e }
  
  implicit def toLiterateSus(string: String) = new LiterateSus(specify(string))
  
  /** This class acts as an extension of a Sus to provide a literate description of a sus as an xml specification */
  class LiterateSus(sus: Sus) {
    def ->>(e: => Elem)= {
      sus.verb = ""
      format(e)
    }
    /** associates every <ex> tag to an anonymous example */
    private def format(e: => Elem) = {
      val content = e
      val anonymous = sus.examples.filter(_.description.matches("example \\d+"))
      val exNodes = content.\("ex")
      exNodes.theSeq.toList.zip(anonymous.toList).foreach { pair =>
        val (node, example) = pair
        example.exampleDescription = if (content.exists(_.label == "wiki")) WikiExampleDescription(node.first.text) else ExampleDescription(node.first.text) 
        List("tag", "tags") foreach { tagName => addTag(node, example, tagName) }
      }
      sus.literateDescription = Some(content)
    }
    private def addTag(node: Node, example: Example, tagName: String) = {
      node.attribute(tagName) match {
        case None => ()
        case Some(a) => a.toString.split(",").foreach(t => example.addTag(t.trim))
      }
   }
 
    /** specifies the system with a literate description and embedded assertions */
    def is(e: => Elem)= {
      sus.verb = "specifies"
      format(e)
    }
  }
  
  /** embeddeds a test into a new example and silence the result */
  def check(test: =>Any) = (forExample in test).shh

  /** return a String containing the output messages from the console with a given padding such as a newline for instance */
  def consoleOutput(pad: String, messages: Seq[String]): String = { pad + consoleOutput(messages) }

  /** return a String containing the output messages from the console */
  def consoleOutput(messages: Seq[String]): String = messages.map("> " + _.toString).mkString("\n")
}
