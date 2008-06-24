package org.specs.runner
import org.specs.io.mock.MockFileSystem
import org.specs.io.ConsoleOutput
import org.specs.runner._

class specsFinderTest extends JUnit3(specsFinderSpec)
object specsFinderSpecRunner extends ConsoleRunner(specsFinderSpec)
object specsFinderSpec extends Specification with Init {
  
  "A specs finder" should { 
    usingBefore { () => {
      finder.defaultExtension = ".scala"
      finder.reset
    } }
    "not find the name of a specification in the file is not a .scala file" in {
      finder.addFile("fileName", packageDeclaration + specificationContent)
      finder.specificationNames("path", ".*") mustBe Nil
    }
    "find the name of a specification if it is matched by a user-defined pattern" in {
      finder.addFile(packageDeclaration + specificationContent)
      finder.specificationNames("path", "trueSpec") mustContain "org.specs.trueSpec$"
      finder.specificationNames("path", "tr.*Spec") mustContain "org.specs.trueSpec$"
      finder.specificationNames("path", "gloups.*Spec") mustNotContain "org.specs.trueSpec$"
    }
    "find the name of a specification in a specification file" in {
       finder.addFile(packageDeclaration + specificationContent)
       finder.specificationNames("path", ".*") mustExistMatch "trueSpec"
    }
    "create the name of a specification with its package name" in {
      finder.addFile(packageDeclaration + specificationContent)
      finder.specificationNames("path", ".*") mustContain "org.specs.trueSpec$"
    }
    "create the name of a specification with its package name if the package declaration ends with a;" in {
      finder.addFile(packageDeclarationWithSc + specificationContent)
      finder.specificationNames("path", ".*") mustContain "org.specs.trueSpec$"
    }
    "create the name of a specification with no package name for a file with no package declaration" in {
      finder.addFile(specificationContent)
      finder.specificationNames("path", ".*") mustContain "trueSpec$"
    }
    "return an empty list if there is no specification declaration found in the file" in {
      finder.addFile(packageDeclaration)
      finder.specificationNames("path", ".*") must_== List()
    }
    "return a list with 2 specification names if the file contains 2 specs" in {
      finder.addFile(packageDeclaration + specificationContent + specificationContent)
      finder.specificationNames("path", ".*") must_== List("org.specs.trueSpec$", "org.specs.trueSpec$")
    }
    "return a list with 2 specification names if run over a directory with 2 files" in {
      finder.addFile("file1.scala", packageDeclaration + specificationContent)
      finder.addFile("file2.scala", packageDeclaration + specificationContent)
      finder.specificationNames("dir1", ".*") must_== List("org.specs.trueSpec$", "org.specs.trueSpec$")
    }
  }
  object finder extends MockFileSystem with SpecsFinder with ConsoleOutput
}
trait Init {
  val packageDeclaration = "package org.specs"
  val packageDeclarationWithSc = packageDeclaration + ";"
  val specificationContent = """
    object trueSpec extends Specification with MockOutput {
      "A specification" should {
        "have example 1 ok" in { true }
        }
      }
    """
}

