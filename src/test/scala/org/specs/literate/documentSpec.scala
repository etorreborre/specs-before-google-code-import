package org.specs.literate
import org.specs.specification._
import org.specs.{ Specification }
import scala.xml._

class UnifiedDocumentSpec extends org.spex.Specification with DocumentsFactory {
  "A simple specification with documents" should {
    val s = new Specification {
      "This sus" should {
        "have an introduction".txt
        "have an example" in { 1 must_== 1 }
      }
    }
    "allow the creation of descriptions in between examples" in {
      s.systems(0).documents must have size(2)
    }
  }
}
class DocumentSpec extends org.spex.Specification with DocumentsFactory {

  var sus = new Sus(this)
  val doc = new DocumentSample
  "A specification with documents" should {
    "have a default sus" in {
      doc.systems must have size 2
    }
    "display a String when using a HtmlRunner" in {
      docString must include("Documents can be created")
    }
    "display a formatted String with Markdown" in {
      docString must include("<em>If</em>")
    }
    "have a \\\\ operator to append a document" in {
      docString must include("together.</p><p><md><em>If")
    }
    "have a \\ operator to inline an example" in {
      doc.asHtml(doc) must \\("ex")
    }
    "allow break returns between docs" in {
      ("hello".md \- br -\ "world" toXhtml).toString must include("hello</md></p><br></br><p>world")
    }
  }
  def docString = docBody toString
  def docBody = doc.asHtml(doc) \\ "body" 
    
  "A Document" should {
    "have a \\ method to add another document" in {
      ("hello" \ " world" toXhtml).toString must_== "hello world" 
    }
    "have a \\\\ method to create 2 paragraphs and add them" in {
      ("hello" \\ "world" toXhtml).toString must_== "<p>hello</p><p>world</p>" 
    }
    "have a :\\ method to create a paragraph for the second doc and add it to the first" in {
      ("hello" -\ "world" toXhtml).toString must_== "hello<p>world</p>" 
    }
    "have a \\: method to create a paragraph for the first doc and add it to the second" in {
      ("hello".md \- "world" toXhtml).toString must_== "<p><md>hello</md></p>world" 
    }
  }
  "A Markdown document" should {
    "have a toXhtml method displaying the document" in {
      "hello".md.toXhtml.toString must_== "<md>hello</md>" 
    }
  }
  "A document sequence" should {
    "have a toXhtml method calling the other documents toXhtml methods" in {
      ("hello" \ " beautiful" \ " world").toXhtml.toString must_== "hello beautiful world"
    }
  }
}

import org.specs.runner._
import scala.xml._
class DocumentSample extends LiterateSpecification with Documents with org.specs.runner.Html {

  "Documents can be created by creating small document pieces and appending them together." \\
  "_If_ the firt document is created outside of any sus, then a sus is created for it.".md \ br \ 
  "Then this document can embbed examples" \
  "like this one".in { 1 must_== 1 } \\
  "or this other" \
  "one here".in { 1 must_== 1 } \
  "And the text can resume afterwards to present the rest of the specification."

  "Documents can also be added inside a sus. A sus with documents" can {
    "have some description text" \\
    "with paragraphs if necessary"
    "and examples which are not necessarily related to the rest of the doc" in { 1 must_== 1 }
  }

}