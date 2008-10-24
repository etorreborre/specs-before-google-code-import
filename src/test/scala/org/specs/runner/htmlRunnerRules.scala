package org.specs.runner
 
import org.specs.specification._
import org.specs.Sugar._
import org.specs._
import org.specs.util._
import org.specs.io.mock._
import scala.xml._

class htmlRunnerRules(name: String) extends LiterateSpecification(name) with XmlProperties {
  
  def title = run must \\(<title>{specification.name}</title>)
  def oneTablePerSus = run must \\(<table></table>)
  def subSpecsHeader = run must \\(<h2>Sample subspecification</h2>)
  def systemName = run must \\(<h3>The system should<a href="#top">   <img src="images/up.gif"/></a></h3>)
  def topArrow = run must (\\(<a href="top"/>) and
                           \\(<a href="#top"><img src="images/up.gif"/></a>))
  def oneRowPerExample = runString must beMatching("ex1")
  def exampleDescription = runString must beMatching("ex1")
  def exampleSuccess = run must \\(<img src="images/icon_success_sml.gif"/>)
  def failedExampleImage = run must \\(<img src="images/icon_failure_sml.gif"/>)
  def errorExampleImage = run must \\(<img src="images/icon_error_sml.gif"/>)
  def skippedExampleImage = run must \\(<img src="images/icon_skipped_sml.gif"/>)
  def failedExample = runString must include("'1' is not equal to '0'")
  def errorExample = runString must beMatching("bug")
  def skippedExample = runString must beMatching("skipped")
  def rowsAlternation = run must (\\(<tr class="a"></tr>) and \\(<tr class="b"></tr>))  
  def outputFile = htmlFile.toString must include("./target/specification.html")
  def cssDir = createdDirs must contain("./target/css")
  def imagesDir = createdDirs must contain("./target/images") 
  def dataTableFailure = run must (\\(<td>a</td>) and \\(<td>b</td>) and \\(<td>result</td>))
  def literateDesc = run must \\("h1")
  def subExamples = run must (beMatching("subex1")^^((_: Iterable[Node]).toString) 
                              and \\(<h4>this example has sub-examples</h4>))   
  def systemsList = run must (\\(<div id="leftColumn"/>) and \\(<td>{specification.name}</td>))
  def noSystemsListForOneOnly = runSmall.toString must include("onLoad=\"noNavBar()\"")
  def collapsibleColumn = run must \\(<img src="images/expanded.gif" onclick="toggleNavBar(this)"/>)
  def greenHighlight = 1 must_== 1
    
  lazy val executeRunner = { 
    hRunner.report(hRunner.specs)
    hRunner
  } 
  lazy val executeSmallRunner = { 
    smallRunner.report(smallRunner.specs)
    smallRunner
  } 
  val run = executeRunner.results aka "the generated html"
  val runString = executeRunner.results.toString aka "the generated html"
  val runSmall = executeSmallRunner.results

  val htmlFile = executeRunner.files.keySet.filter(_.toString.endsWith(".html")).first aka "the report file"
  val createdDirs = executeRunner.createdDirs aka "the list of created directories"

  object smallRunner extends InstrumentedRunner(subSpecification)
  object hRunner extends InstrumentedRunner(specification)
  class InstrumentedRunner(spec: Specification) extends org.specs.runner.HtmlRunner(spec, "target/", HtmlNamingFunction.short) with Console with MockOutput with MockFileSystem {
    var results: Elem = _
    override def copySpecResourcesDir(src: String, outputDir: String) = {
      mkdirs("./target/css")
      mkdirs("./target/images")
      addFile("./target/css/maven-base.css", "")
      addFile("./target/images/success.gif", "")
    }
    override def specOutput(spec: Specification) = {
      results = asHtml(spec)
      new PrettyPrinter(200, 2).format(results)
    }
    
  }
}
import org.specs.specification._

object specification extends LiterateSpecification("Sample Specification") {
    include(subSpecification)
    "this literate sus" is 
<wiki>h1. A h1 title
  {1 must_== 1}
  {  
    val calc = new Object { def add(x: Int, y: Int): Int = x + y }
    "A calculator can add integers" inTable 
    "a" | "b" | "c" | 
     1  !  2  !  3  |
     2  !  2  !  5  |
     2  !  6  !  8  |> { (a:Int,b:Int,c:Int) => c must_== calc.add(a, b) } 
  }

</wiki>

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
import org.specs.runner._
class htmlRunnerSpecTest extends HtmlSuite(htmlRunnerSpec, "target") with JUnit
