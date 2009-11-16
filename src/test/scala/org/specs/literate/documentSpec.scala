package org.specs.literate

import org.specs.xml.Xhtml._

class documentSpec extends Specification with Documents {
  "An Example doc as text" should {
    "display the description of the example" in {
      s.success.toText must_== "success 1"
    }
  }
  "An Example doc as xhtml" should {
    "display the description of the example" in {
      s.success.toXhtml must_== <ex class="info">success 1</ex>
    }
  }
  "An Example doc as executed text" should {
    behave like "An Example doc as text"
    "display the status of the example" in {
      s.success.toText_! must_== "+ success 1"
    }
  }
  "An Example doc as executed xhtml" should {
    behave like "An Example doc as xhtml"
    "display the status of the example" in {
      s.success.toXhtml_! must_== <ex class="success">success 1</ex>
    }
  }
  "An example followed by another example, as text" should {
    "display examples on one line with \\" in {
      (s.success \ s.success).toText must_== "success 1success 1" 
    }
    """display examples with a space with \" "\ """ in {
      (s.success \" "\ s.success).toText must_== "success 1 success 1" 
    }
    """display examples on a new line \br\ """ in {
      (s.success \br\ s.success).toText must_== "success 1\nsuccess 1" 
    }
    """display examples on one paragraph each with \p\ """ in {
      (s.success \p\ s.success).toText must_== "success 1\n\nsuccess 1" 
    }
  }
  "An example followed by another example, as executed xhtml" should {
    "display examples on one line with \\" in {
      (s.success \ s.success).toXhtml_!.toString must_== 
        """<ex class="success">success 1</ex><ex class="success">success 1</ex>"""
    }
    """display examples with a space with \" "\ """ in {
      (s.success \" "\ s.success).toXhtml_!.toString must_== 
        """<ex class="success">success 1</ex> <ex class="success">success 1</ex>"""
    }
    """display examples on a new line \br\ """ in {
      (s.success \br\ s.success).toXhtml_!.toString must_== 
        """<ex class="success">success 1</ex><br></br><ex class="success">success 1</ex>"""
    }
    """display examples on one paragraph each with \p\ """ in {
      (s.success \p\ s.success).toXhtml_!.toString must_== 
        """<ex class="success">success 1</ex><br></br><br></br><ex class="success">success 1</ex>"""
    }
  }
  "A sus doc as text" should {
    "display the examples as a bullet list" in {
      s.sus.toText must_==
      """|sus should
         |  - success 1
         |  - failure 1""".stripMargin
    }
  }
  "A sus doc as xhtml" should {
    "display the examples as a ul list" in {
      s.sus.toXhtml.toHtml must_==
      """|<sus>
         |  <t>sus should</t>
         |  <ul>
         |    <li>
         |      <ex class="info">success 1</ex>
         |    </li>
         |    <li>
         |      <ex class="info">failure 1</ex>
         |    </li>
         |  </ul>
         |</sus>""".stripMargin
    }
  }
  "A sus doc as executed text" should {
    "display the examples as a bullet list with their status" in {
      s.sus.toText_! must_==
      """|x sus should
         |  + success 1
         |  x failure 1""".stripMargin
    }
  }
  "A sus doc with an introductory text" should {
    "display the introduction before the examples" in {
      noDetailedDiffs()
      s.susWithIntro.toText aka s.susWithIntro.toString must_==
      """|This is an intro text to
         |
         |sus should
         |  - success 1
         |  - failure 1""".stripMargin
    }
  }
}
object s extends Specification with Documents
{
  val success = "success 1" in { 1 must_== 1 } 
  val sus = "sus" should {
    "success 1" in { 1 must_== 1 }
    "failure 1" in { 1 must_== 2 }
  }
  val susWithIntro = "This is an intro text to" \\
  "sus". should {
    "success 1" in { 1 must_== 1 }
    "failure 1" in { 1 must_== 2 }
  }
}

