package org.specs.runner

object htmlRunnerSpec extends htmlRunnerRules { "the html runner specification" is <spec>
  
  A specification can be run and its output displayed as an Html page.
  
    1. Specification Title
    
    <ex>The title of the html page should be the title of the specification.</ex>{title}
    
    2. Specifications headers
    
    <ex>There should be one header per specification</ex>{subSpecsHeader}

    3. System tables
    
    <ex>There should be a table for each system under test.</ex>{oneTablePerSut}
    <ex>The table must be preceded by the system name as a separate header.</ex>{systemName}
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

    5. Output directory
    
       4.1 File name
      
       The output of an HtmlRunner can be specified by specifiying an output directory.
       In that case, <ex>the runner generates a file named specs-report.html in that directory.</ex>{outputFile} 
    
       4.2 Stylesheets and images
      
       <ex>The stylesheets for the report must be created in a directory named css, relative to the output directory.</ex>{cssDir} 
       <ex>The images for the report must be created in a directory named images, relative to the output directory.</ex>{imagesDir} 
   
</spec>
}
 
import org.specs.specification._
import org.specs.Sugar._
import org.specs.io.mock._
trait htmlRunnerRules extends LiterateSpecification {
  def title = run must \\(<title>{specification.name}</title>) <|
  def oneTablePerSut = run must \\(<table></table>) <|
  def subSpecsHeader = run must \\(<h1>Sample subspecification</h1>) <|
  def systemName = run must \\(<h3>The system should</h3>) <|
  def oneRowPerExample = run.toString must beMatching("ex1") <|
  def exampleDescription = run.toString must beMatching("ex1") <|
  def exampleSuccess = run must \\(<img src="images/icon_success_sml.gif"/>) <|
  def failedExampleImage = run must \\(<img src="images/icon_warning_sml.gif"/>) <|
  def errorExampleImage = run must \\(<img src="images/icon_error_sml.gif"/>) <|
  def skippedExampleImage = run must \\(<img src="images/icon_info_sml.gif"/>) <|
  def failedExample = run must \\(<td>'1' is not equal to '0'</td>) <|
  def errorExample = run.toString must beMatching("bug") <|
  def skippedExample = run.toString must beMatching("skipped") <|
  def rowsAlternation = run must (\\(<tr class="a"></tr>) and \\(<tr class="b"></tr>)) <|  
  def outputFile = htmlFile must_== "./target/specs-report.html" <|
  def cssDir = createdDirs must contain("./target/css") <|
  def imagesDir = createdDirs must contain("./target/images") <|
    
  object specification extends Specification("Sample Specification") {
    include(subSpecification)
    "The system" should {
      "ex1" in { 1 must_== 1 }
      "ex2" in { 1 must_== 0 }
      "ex3" in { error("bug") }
      "ex4" in { skip("skipped") }
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
    runner.reportSpecs
    runner
  } 
  val run = executeRunner.specOutput
  val htmlFile = executeRunner.files.keySet.elements.next
  val createdDirs = executeRunner.createdDirs
  object runner extends org.specs.runner.HtmlRunner(specification, "target/") with MockOutput with MockFileSystem {
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
object allSpecsRunner extends org.specs.runner.HtmlRunner(org.specs.matcher.allMatchersUnit, "target/allSpecs/")
