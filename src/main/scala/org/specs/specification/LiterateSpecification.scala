/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.specification
import scala.xml._
import org.specs.util.Property
import org.specs.util.DataTables
import org.specs.util._
import org.specs.Sugar._
import org.specs.matcher._
import org.specs.runner._
import org.specs.form._
import org.specs.execute._

/**
 * This trait supports writing literate specifications for acceptance testing.
 * 
 * The basic idea is to define Systems under specification as some text, enclosed in xml tags:
 * <code>
 * "my system" is <text>
 *   some text for my Specification
 * 
 * </text>
 * </code>
 * 
 * Then, it is possible to "tag" parts of this text with the <ex></ex> xml nodes, to denote an example description and make it immediately
 * followed by an expectation in Scala code: <code>
 * "my system" is <text>
 *   <ex>This is an ok example</ex> { 1 must be equalTo(1) }
 * 
 *   <ex>This is an ok example with several expectations which must be enclosed in an 'eg' function</ex> 
 *   { eg { 
 *       1 must be equalTo(1) 
 *       2 must be equalTo(2) 
 *     }
 *   }
 *   <ex>This is a example but missing its implementation yet</ex> { notImplemented }
 * 
 * </text>
 * </code>
 * 
 */
class LiterateSpecification extends Specification with LiterateBaseSpecification with LiterateSpecificationLinks 
      with LiterateDataTables with LiterateForms with LiterateProperties with LiterateShortcuts {
  setSequential()

  def this(n: String) = { this(); name = n; description = n; this }

  /** allow empty sus to be reported. */
  override def filterEmptySus = false
}
/**
 * LiterateSpecification with Html reporting
 */      
class HtmlSpecification extends LiterateSpecification with Html {
  def this(n: String) = { this(); name = n; description = n; this }
}
/**
 * This trait helps declaring datatables inside the Literate Specification
 */
trait LiterateDataTables extends DataTables with ExpectableFactory with BaseSpecification {
  /**
   * This method allows to embbed a DataTable in a literate specification and display the results of its execution
   */
  implicit def makeTable(s: String) = new TableExample(s)
  case class TableExample(desc: String) {
    def inTable(table: =>ExecutableDataTable) = {
      lazy val tableToExecute = table
      forExample(desc) in {
        isExpectation(tableToExecute.execute)
        if (!tableToExecute.isOk)
          throw new DataTableFailureException(tableToExecute)
      }
      desc + "\n" + tableToExecute.toHtml.toString
    }
  }
}
/**
 * This trait adds shortcut to declare properties in the specification text
 */
trait LiterateProperties extends Properties with ExpectableFactory {
  def field[T](label: String, value: =>T): Field[T] = {
    Field(label, value)
  }
  def displayField[T](label: String, value: =>T) = {
    field(label, value).toHtml
  }
  def prop[T](label: String, actual: =>T): MatcherProp[T] = {
    Prop(label, actual, MatcherConstraint((m:Matcher[T]) => actual must m))
  }
  def displayProp[T](label: String, actual: =>T)(expected: T) = {
    prop(label, actual)(expected).display_!
  }
}
/**
 * This trait adds shortcut to declare forms in the specification text
 */
trait LiterateForms extends ExpectableFactory with BaseSpecification { 
  /**
   * This method allows to embbed a Form in a literate specification and display the results of its execution
   */
  implicit def makeForm(s: String) = new LiterateForm(s)
  case class LiterateForm(desc: String) {
    def inForm(form: =>org.specs.form.Form) = {
      lazy val formToExecute = form
      val description = if (desc.isEmpty) form.title else desc
      forExample(description) in {
          isExpectation(formToExecute.execute)
          if (!formToExecute.isOk) throw new FailureException("The form '" +  formToExecute.title + "' failed")
      }
      description + "\n" + formToExecute.toHtml.toString
    }
  }
}
/**
 * This trait adds shortcut methods to define expectations, to silence expressions
 */
trait LiterateShortcuts extends ExpectableFactory with BaseSpecification with FailOrSkip { 
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
   * Create an anonymous example with a function on a System,
   * giving it a number depending on the existing created examples
   */
  def eg[S](function: S => Any): Unit = (forExample in function).shh

  /** embeddeds a test into a new example and silence the result */
  def eg(test: =>Any): Unit = (forExample in test).shh
  /** create an anonymous example which will be skipped until it is implemented */
  def notImplemented = forExample in { skip("PENDING: not yet implemented") }
  /** return a String containing the output messages from the console with a given padding such as a newline for instance */
  def consoleOutput(pad: String, messages: Seq[String]): String = { pad + consoleOutput(messages) }
  /** return a String containing the output messages from the console */
  def consoleOutput(messages: Seq[String]): String = messages.map("> " + _.toString).mkString("\n")
}
trait LiterateBaseSpecification extends ExpectableFactory with BaseSpecification {
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
}
/**
 * This trait allows to add links to other specifications inside a literate specification.
 * The link will be displayed as a Html link
 * 
 * The "parent/child" relationship is kept in that trait to allow the Html runner
 * to be reported when reporting the parent.
 */
trait LiterateSpecificationLinks extends LinkedSpecification with Links { this: Specification => 
  def linkTo(subSpec: Specification with Html): String = linkTo(subSpec.description, subSpec)
  def linkTo(desc: String, subSpec: Specification with Html): String = {
    super.linkTo(desc, subSpec)
    // execute the subSpec
    subSpec.failures
    relativeLink(desc, subSpec.fileName(subSpec))
  }
}
/**
 * This trait allows to add links to other specifications inside a literate specification.
 * The link will be displayed as a Html link
 * 
 * The "parent/child" relationship is kept in that trait to allow the Html runner
 * to be reported when reporting the parent.
 */
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

  protected def htmlize(s: String) = s.replace("<", "&lt;").replace(">", "&gt;")
  /**
   * Alias for wikiCode
   */
  def >@(stringToFormat: String) = wikiCode(stringToFormat)

  def linkTo(susName: String) = "link to " + susName + " not implemented yet"
  override def pathLink(desc: String, path: String) = {
    "\"" + desc + "\":file:///" + path
  }
  override def relativeLink(desc: String, path: String) = {
    "\"" + desc + "\":" + path
  }
}
trait Textile extends Wiki
trait Markdown extends Wiki {
  override def wikiCode(stringToFormat: String) = stringToFormat.replace("\r\n", "\n").
                                                        replace("\n\r", "\n").
          split("\n").map(htmlize(_)).mkString("<code class=\"prettyprint\">", "</code\n<code class=\"prettyprint\">", "</code>")
  override def pathLink(desc: String, path: String) = {
    "[" + desc + "](file:///" + path + ")"
  }
  def pathLink(desc: String, path: String, title: String) = {
    "[" + desc + "](file:///" + path + " " + title + ")"
  }
  override def relativeLink(desc: String, path: String) = {
    "[" + desc + "](" + path + ")"
  }
  def relativeLink(desc: String, path: String, title: String) = {
    "[" + desc + "](" + path + " " + title + ")"
  }
}
trait Links {
  def pathLink(desc: String, path: String) = desc + " (See: " + path + ")"
  def relativeLink(desc: String, path: String) = desc + " (See: " + path + ")"
}
trait LiterateSnippets extends SnipIt with ExpectableFactory with Matchers { 
  def executeIs(s: String) = { execute(it) must include(s) }
  def executeIsNot(s: String) = execute(it) mustNot include(s)
  implicit def toOutputSnippet(s: String) = OutputSnippet(s)
  case class OutputSnippet(s: String) {
    def add_> = {
      s add it
      "> " + execute(it)
    } 
  }
  def >(s: String) = outputIs(s)
  def outputIs(s: String) = {
    val result = execute(it)
    var out = "> " + result
    try  { result must include(s) }
    catch { case e => out = "> " + e.getMessage }
    out
  }
}