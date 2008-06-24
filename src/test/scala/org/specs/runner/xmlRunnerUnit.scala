package org.specs.runner
import org.specs.specification._
import org.specs.util.DataTables
import org.specs.Sugar._

class xmlRunnerUnitTest extends JUnit4(xmlRunnerUnit)
object xmlRunnerUnit extends LiteralSpecification with TestData {
  "An xml runner" should {
    "create an xml file in the default directory if nothing is specified" in {
       xmlRunner.reportSpec
       xmlRunner.files must haveKey("./spec1.xml")
    }
    "create an xml file in the specified output directory, handling file separators" in {
       "output dir" | 	"spec name" | 	"file path"  				|>
       "" 		    ! 	"spec1" 	!	"./spec1.xml"				|  
       "result" 	!	"spec1" 	!	"./result/spec1.xml" 		|  
       "result/" 	!	"spec1" 	!	"./result/spec1.xml" 		|  
       "result\\" 	!	"spec1" 	!	"./result/spec1.xml" 		|  
       "/result" 	!	"spec1" 	!	"/result/spec1.xml" 		|
       "\\result" 	!	"spec1" 	!	"/result/spec1.xml" 		|
       "result/xml" ! 	"spec1"     !	"./result/xml/spec1.xml"	| {
       (dir: String, 	spec: String, 	result: String) => {
           xmlRunner.reset
           xmlRunner.outputDir = dir; spec1.name = spec;
           xmlRunner.reportSpec
           xmlRunner.files.keySet must contain(result)
         }
       }
       
    }
  }
}
trait TestData extends DataTables {
  import org.specs.io.mock._
  trait ExtendedMockFileSystem extends MockFileSystem {
    override def createFile(path: String) = {files += (path -> ""); true}
   }
  object xmlRunner extends XmlRunner(spec1) with ExtendedMockFileSystem with MockOutput
  object spec1 extends Specification {
    override def toString = name
    "the sut" should { "have one ok example" in { 1 mustBe 1 } }
  } 
}

