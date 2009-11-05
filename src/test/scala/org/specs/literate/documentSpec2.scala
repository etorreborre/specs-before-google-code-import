
package org.specs.literate

class documentSpec2 extends Specification with Documents {
  "An Example doc as text" should {
    "display the description of the example" in {
      s.success.toText must_== "success 1"
    }
  }
  "An Example doc as executed text" should {
    behave like "An Example doc as text"
    "display the status of the example" in {
      s.success.toText_! must_== "+ success 1"
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
  "An sus doc as text" should {
    "display the examples as a bullet list" in {
      s.sus.toText must_==
      """|sus should
         |  - success 1
         |  - failure 1""".stripMargin
    }
  }
  "An sus doc as executed text" should {
    "display the examples as a bullet list with their status" in {
      s.sus.toText_! must_==
      """|x sus should
         |  + success 1
         |  x failure 1""".stripMargin
    }
  }
}
object s extends Specification
{
  val success = "success 1" in { 1 must_== 1 } 
  val sus = "sus" should {
    "success 1" in { 1 must_== 1 }
    "failure 1" in { 1 must_== 2 }
  }
}

