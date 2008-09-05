package org.specs.runner
import org.specs.specification._
import org.specs.util.DataTables
import org.specs.Sugar._

class xmlRunnerUnitTest extends HtmlSuite(xmlRunnerUnit, "./target/xmlRunnerUnit") with JUnit
object xmlRunnerUnit extends Specification with DataTables {
  "An xml runner" should {
    "create an xml file in the default directory if nothing is specified" in {
       xRunner.reportSpecs
       xRunner.files must haveKey("./spec1.xml")
    }
    "create an xml file in the specified output directory, handling file separators" in {
       "output dir" | 	"spec name" | 	"file path"  				|>
       "" 		    ! 	"spec1" 	!	"./spec1.xml"				|  
       "result" 	!	"spec1" 	!	"./result/spec1.xml" 		|  
       "result/" 	!	"spec1" 	!	"./result/spec1.xml" 		|  
       "result\\" 	!	"spec1" 	!	"./result/spec1.xml" 		|  
       "/result" 	!	"spec1" 	!	"/result/spec1.xml" 		|
       "\\result" 	!	"spec1" 	!	"/result/spec1.xml" 		|
       "result/xml" ! 	"spec1"     !	"./result/xml/spec1.xml"	| { (dir, spec, result) =>
           xRunner.reset
           xRunner.setOutputDir(dir)
           spec1.name = spec
           xRunner.reportSpecs
           xRunner.files.keySet must contain(result)
       }
    }
  }
}
import org.specs.io.mock._
trait ExtendedMockFileSystem extends MockFileSystem {
  override def createFile(path: String) = {files += (path -> ""); true}
}
object xRunner extends XmlRunner(spec1, "result", XmlNamingFunction.short) with ExtendedMockFileSystem with Console with MockOutput {
  var dirPath = ""
  def setOutputDir(dir: String) = dirPath = dir
  override def outputDir = dirPath
}
object spec1 extends Specification {
  "the sus" should { "have one ok example" in { 1 mustBe 1 } }
}
