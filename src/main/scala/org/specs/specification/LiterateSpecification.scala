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
class LiterateSpecification extends Specification with ExpectableFactory with DataTables {
  setSequential
  def this(n: String) = { this(); name = n; description = n; this }
  
  /**
   * This method is used to silence the result of a call in an action. For example: <pre>
   * The timer should be stopped {timer.stop.shh}
   * </pre>. This will not output the result of the stop method
   */
  implicit def anyToShh(a: Any) = new Silenced
  
  class Silenced {
    def shh = () 
    
    /** the pipe bar must be interpreted visually as a stop and the < sign as a pike. */
    def <| = shh
  }
  /** This silence function allows to silence calls with this style: shh { a call } */
  def shh(a: =>Any) = { a; () }

  /**
   * This method is used setup a property value, in order to avoid repeting a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = new AsProperty(a)
  implicit def propertyToValue[T](p: Property[T]):T = p()
  case class AsProperty[T](a: T) { 
    def as(p: Property[T]) = {p() = a; a }
    def apply(p: Property[T]) = {p() = a; a}
    def apply(f: T => Any)= {f(a); a }
    def as(f: T => Any)= {f(a); a }
  }
  
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
  }    
  
  /** create an anonymous example which will be skipped until it is implemented */
  def notImplemented = forExample in { skip ("not implemented yet")}
  implicit def toSus(e: => Elem) = new Object { def isSus = toLiterateSus("") ->> e }
  
  implicit def toLiterateSusWithDesc(string: String) = new LiterateSus(specify(string))
  implicit def toLiterateSus(sus: Sus) = new LiterateSus(sus)
  
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
 
    /** specifies the system with a literate description and embedded expectations */
    def is(e: => Elem)= {
      sus.verb = "specifies"
      format(e)
    }
  }
  
  /**
   * Create an anonymous example with a function on a System, 
   * giving it a number depending on the existing created examples/
   */
  def eg[S](function: S => Any): Unit = (forExample in function).shh
  /** 
   * embeddeds a test into a new example and silence the result
   * @deprecated
   */
  def check[S](function: S => Any): Unit = eg(function)

  /** embeddeds a test into a new example and silence the result */
  def eg(test: =>Any): Unit = (forExample in test).shh
  /** 
   * embeddeds a test into a new example and silence the result
   * @deprecated
   */
  def check(test: =>Any): Unit = eg(test)

  /** return a String containing the output messages from the console with a given padding such as a newline for instance */
  def consoleOutput(pad: String, messages: Seq[String]): String = { pad + consoleOutput(messages) }

  /** return a String containing the output messages from the console */
  def consoleOutput(messages: Seq[String]): String = messages.map("> " + _.toString).mkString("\n")

  def includeSus(susName: String) = "include " + susName + " not implemented yet"
}
/**
 * This trait provides String properterties with alphabetical names.
 */
trait StringProperties { outer =>
  val a = Property[String]("")
  val b = Property[String]("")
  val c = Property[String]("")
  val d = Property[String]("")
  val e = Property[String]("")
  val f = Property[String]("")
  implicit def stringToAlpha(value: String) = StringToAlpha(value)
  case class StringToAlpha(value: String) {
    def a = { outer.a() = value; value }
    def b = { outer.b() = value; value }
    def c = { outer.c() = value; value }
    def d = { outer.d() = value; value }
    def e = { outer.e() = value; value }
    def f = { outer.f() = value; value }
  } 
}
/**
 * This trait provides Xml properterties with alphabetical names.
 */
trait XmlProperties { outer =>
  val xml  = Property[Elem](<e/>)
  val xml2 = Property[Elem](<e/>)
  val xml3 = Property[Elem](<e/>)
  val xml4 = Property[Elem](<e/>)
  val xml5 = Property[Elem](<e/>)
  val xml6 = Property[Elem](<e/>)
  implicit def elemToAlpha(value: Elem) = ElemToAlpha(value)
  case class ElemToAlpha(value: Elem) {
    def abc  = { outer.xml() = value; value }
    def xml2 = { outer.xml2() = value; value }
    def xml3 = { outer.xml3() = value; value }
    def xml4 = { outer.xml4() = value; value }
    def xml5 = { outer.xml5() = value; value }
    def xml6 = { outer.xml6() = value; value }
  } 
}
/**
 * This trait provides Int properterties with alphabetical names.
 */
trait IntProperties { outer =>
  val i = Property[Int](0)
  val j = Property[Int](0)
  val k = Property[Int](0)
  val l = Property[Int](0)
  val m = Property[Int](0)
  val n = Property[Int](0)
  implicit def intToAlpha(value: Int) = IntToAlpha(value)
  case class IntToAlpha(value: Int) {
    def i = { outer.i() = value; value }
    def j = { outer.j() = value; value }
    def k = { outer.k() = value; value }
    def l = { outer.l() = value; value }
    def m = { outer.m() = value; value }
    def n = { outer.n() = value; value }
  } 
}
/**
 * This trait provides Boolean properterties with alphabetical names.
 */
trait BooleanProperties { outer =>
  val o = Property[Boolean](true)
  val p = Property[Boolean](true)
  val q = Property[Boolean](true)
  val r = Property[Boolean](true)
  val s = Property[Boolean](true)
  val t = Property[Boolean](true)
  implicit def booleanToAlpha(value: Boolean) = BooleanToAlpha(value)
  case class BooleanToAlpha(value: Boolean) {
    def o = { outer.o() = value; value }
    def p = { outer.p() = value; value }
    def q = { outer.q() = value; value }
    def r = { outer.r() = value; value }
    def s = { outer.s() = value; value }
    def t = { outer.t() = value; value }
  } 
}
/**
 * This trait provides Double properterties with alphabetical names.
 */
trait DoubleProperties { outer =>
  val u = Property[Double](0.0)
  val v = Property[Double](0.0)
  val w = Property[Double](0.0)
  val x = Property[Double](0.0)
  val y = Property[Double](0.0)
  val z = Property[Double](0.0)
  implicit def doubleToAlpha(value: Double) = DoubleToAlpha(value)
  case class DoubleToAlpha(value: Double) {
    def u = { outer.u() = value; value }
    def v = { outer.v() = value; value }
    def w = { outer.w() = value; value }
    def x = { outer.x() = value; value }
    def y = { outer.w() = value; value }
    def z = { outer.z() = value; value }
  } 
}
/**
 * This trait one String property for a current value.
 */
trait CurrentProperty { outer => 
  val it = Property[String]("")
  implicit def stringToIt(s: String) = StringToIt(s)
  case class StringToIt(s: String) {
    def it = { outer.it() = s; s }
  } 
  
}
/**
 * This trait adds all properties.
 */
trait AllProperties extends StringProperties with IntProperties with DoubleProperties with BooleanProperties with CurrentProperty
object AllProperties extends AllProperties
/**
 * This trait provides functions which can be used to ease the use of wiki markup
 */
trait Wiki {
  implicit def toWikiString(a: Any) = new WikiString(a.toString) 
  class WikiString(s: String) {
    def >@ = wikiCode(s)
  }
  /** 
   * This function can be used to format code in a wiki description.
   * Using this function avoid issues like quotes insides brackets ['something']
   * being displayed as question marks.
   */
  def wikiCode(stringToFormat: String) = "<code>"+stringToFormat+"</code>"
  /** 
   * Alias for wikiCode
   */
  def >@(stringToFormat: String) = wikiCode(stringToFormat)

  def linkTo(susName: String) = "link to " + susName + " not implemented yet"
}