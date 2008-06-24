package org.specs.specification
import scala.xml._
import org.specs.util.Property
import org.specs.util.DataTables
import org.specs.util._
import org.specs.Sugar._

/**
 * This trait is experimental. It is supposed to help writing some literal specifications
 * using the xml capabilities of Scala.
 * Several "toy" specifications have been written using this style:<ul>
 * <li>bizSpecificationSpec
 * <li>calculatorBizSpec
 * <li>xmlRunnerSpec
 * </ul>
 */
trait LiteralSpecification  extends Specification with DataTables {
  setSequential
  
  /**
   * This method is used to silence the result of a call in an action. For example: <pre>
   * The timer should be stopped {timer.stop.shh}
   * </pre>. This will not output the result of the stop method
   */
  implicit def anyToShh(a: Any) = new Silenced
  class Silenced {
    def shh = ""
  }
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
   * This method allows to embbed a DataTable in a literal specification and display the results of its execution
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
  implicit def toSut(e: => Elem) = new Object { def isSut = toLiteralSut("") ->> e }
  
  implicit def toLiteralSut(string: String) = new LiteralSut(specify(string))
  
  /** This class acts as an extension of a Sut to provide a literal description of a sut as an xml specification */
  class LiteralSut(sut: Sut) {
    def ->>(e: => Elem)= {
      sut.verb = ""
      format(e)
    }
    private def format(e: => Elem) = {
      val content = e
      val anonymous = sut.examples.filter(_.description.matches("example \\d"))
      val exNodes = content.\("ex")
      exNodes.theSeq.toList.zip(anonymous.toList).foreach( pair => pair._2.description = pair._1.first.text)
      sut.literalDescription = Some(content.text)
    }
    /** specifies the system with a literal description and embedded assertions */
    def is(e: => Elem)= {
      sut.verb = "specifies"
      format(e)
    }
  }
  
  def check(test: =>Any) = (forExample in test).shh
  def consoleOutput(pad: String, messages: Seq[String]): String = { pad + consoleOutput(messages) }
  def consoleOutput(messages: Seq[String]): String = messages.map("> " + _.toString).mkString("\n")
}



