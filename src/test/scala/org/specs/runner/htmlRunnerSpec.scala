package org.specs.runner

object htmlRunnerSpec extends htmlRunnerRules { "the html runner specification" is <spec>
  
  A specification can be run and its output displayed as an Html page.
  On this html page we should be able to see:
    
    -statistics about the specification execution
    -the list of all examples, sorted by sub-specifications and systems
    -an overview list of all sub-specifications and systems with a status icon to allow a rapid access
  
    1. Specification Title
    
    <ex>The title of the html page should be the title of the specification.</ex>{title}
    
    2. Specifications headers
    
    <ex>There should be one header per specification</ex>{subSpecsHeader}

    3. System tables
    
    <ex>There should be a table for each system under test.</ex>{oneTablePerSut}
    <ex>The table must be preceded by the system name as a separate header.</ex>{systemName}
    <ex>Close to the system name, there should be a small "up" arrow to go to the top of the page</ex>{topArrow}
    <ex>In each table, there should be a row per example</ex>{oneRowPerExample}
    
    4. Example rows
    
    On each row, there should be:
      - <ex>the description of the example</ex>{exampleDescription}
      - <ex>a success image if the example succedeed</ex>{exampleSuccess}
      - <ex>a failure image if the example failed</ex>{failedExampleImage}
      - <ex>an error image if the example has an error</ex>{errorExampleImage}
      - <ex>an info image if the example is skipped</ex>{skippedExampleImage}
      - <ex>a failure message if any</ex>{failedExample}
      - <ex>an exception message if any</ex>{errorExample}
      - <ex>a skip message if any</ex>{skippedExample}

    <ex>The rows must alternate in style for better visibility</ex>{rowsAlternation}

        4.1 Subexamples
          
    <ex>An example can also have sub-examples. In that case, the description of the example must be
        displayed as a title and sub-examples displayed in a table</ex>{subExamples}
    
        4.2 DataTables
      
        <ex>DataTables failures should be displayed as an inner table in the message cell</ex>{dataTableFailure}
    
    5. Summary
    
   <ex>A column with the list of systems should be available on the left to access a given system directly</ex>{systemsList}
      
    6. Output directory
    
       6.1 File name
      
       The output of an HtmlRunner can be specified by specifiying an output directory.
       In that case, <ex>the runner generates a file named specs-report.html in that directory.</ex>{outputFile} 
    
       6.2 Stylesheets and images
      
       <ex>The stylesheets for the report must be created in a directory named css, relative to the output directory.</ex>{cssDir} 
       <ex>The images for the report must be created in a directory named images, relative to the output directory.</ex>{imagesDir} 
   
</spec>
}
 
import org.specs.specification._
import org.specs.Sugar._
import org.specs.io.mock._
import scala.xml._

trait htmlRunnerRules extends LiterateSpecification {
  
  def title = run must \\(<title>{specification.name}</title>)
  def oneTablePerSut = run must \\(<table></table>)
  def subSpecsHeader = run must \\(<h2>Sample subspecification</h2>)
  def systemName = run must \\(<h3>The system should<a href="#top">   <img src="images/up.gif"/></a></h3>)
  def topArrow = run must (\\(<a href="top"/>) and
                           \\(<a href="#top"><img src="images/up.gif"/></a>))
  def oneRowPerExample = run.toString must beMatching("ex1")
  def exampleDescription = run.toString must beMatching("ex1")
  def exampleSuccess = run must \\(<img src="images/icon_success_sml.gif"/>)
  def failedExampleImage = run must \\(<img src="images/icon_warning_sml.gif"/>)
  def errorExampleImage = run must \\(<img src="images/icon_error_sml.gif"/>)
  def skippedExampleImage = run must \\(<img src="images/icon_info_sml.gif"/>)
  def failedExample = run must \\(<td>'1' is not equal to '0'</td>)
  def errorExample = run.toString must beMatching("bug")
  def skippedExample = run.toString must beMatching("skipped")
  def rowsAlternation = run must (\\(<tr class="a"></tr>) and \\(<tr class="b"></tr>))  
  def outputFile = htmlFile must_== "./target/specs-report.html"
  def cssDir = createdDirs must contain("./target/css")
  def imagesDir = createdDirs must contain("./target/images") 
  def dataTableFailure = run must (\\(<td>a</td>) and \\(<td>b</td>) and \\(<td>result</td>))
  def subExamples = run must (beMatching("subex1")^^((_: Iterable[Node]).toString) 
                              and \\(<h4>this example has sub-examples</h4>))   
  def systemsList = run must (\\(<div id="leftColumn"/>) and \\(<td>{specification.name}</td>))
    
  object specification extends Specification("Sample Specification") {
    include(subSpecification)
    "The system" should {
      "ex1" in { 1 must_== 1 }
      "ex2" in { 1 must_== 0 }
      "ex3" in { error("bug") }
      "ex4" in { skip("skipped") }
      "data table failure" in {
        "a"    | "b"  | "result" |>
          1    !  1   ! 2        |
          1    !  1   ! 2        |
          3    !  1   ! 5        | { (a: Int, b: Int, c: Int) => 
            a + b must_== c 
          }
      }
      "this example has sub-examples" in {
         "subex1" in { 1 must_== 1 }
         "subex2" in { 1 must_== 1 }
      } 
    }
  }
  object subSpecification extends Specification("Sample subspecification") {
    "The system" should {
      "ex1" in { 1 must_== 1 }
      "ex2" in { 1 must_== 0 }
      "ex3" in { error("bug") }
      "ex4" in { skip("skipped") }
    }
  }
  lazy val executeRunner = { 
    hRunner.reportSpecs
    hRunner
  } 
  val run = executeRunner.specOutput
  val htmlFile = executeRunner.files.keySet.elements.next
  val createdDirs = executeRunner.createdDirs
  object hRunner extends org.specs.runner.HtmlRunner(specification, "target/") with MockOutput with MockFileSystem {
    override def copySpecResourcesDir(src: String, outputDir: String) = {
      mkdirs("./target/css")
      mkdirs("./target/images")
      addFile("./target/css/maven-base.css", "")
      addFile("./target/images/success.gif", "")
    }
  }
}
class htmlRunnerTest extends org.specs.runner.JUnit4(htmlRunnerSpec)
object realRunner extends org.specs.runner.HtmlRunner(htmlRunnerSpec.specification, "target/")
object allSpecsRunner extends org.specs.runner.HtmlRunner(org.specs.allSpecsAndUnits, "target/allSpecs/")
