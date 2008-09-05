package org.specs.runner
import org.specs.specification._
import org.specs.util.Property
import scala.xml._


trait RunnerFixture extends LiterateSpecification with RunnerTestData {
  def createSimpleSpecRunner = runner = simpleSpecRunner
  def executeCompositeSpecRunner = { runner = compositeSpecRunner; executeRunner }
  def executeRunner = { runner.reset; runner.reportSpecs.shh }
  def runnerOutputDir = { runner.setOutputDir _ }
  def checkXml = XML.loadString(runner.readFile(runner.files.keys.next)) must \\(xml()) 
 
  def checkFilePath = {
    createSimpleSpecRunner
    executeRunner
    runner.files must haveKey(path())
  }
  def checkOutputDirectory = {
    runner.reset
    executeRunner
    runner.files must haveKey(path.toString)
  }
  def checkConsole = simpleSpecRunner.messages must not(beEmpty)
}
trait RunnerTestData {
  import org.specs.io.mock._
  import org.specs.io._
  var path = Property("")
  var xml: Property[Elem] = Property(<p></p>).onToString(e => new PrettyPrinter(200, 2).format(e))
  trait OutputDirSettable extends File {
    var dirPath: String = ""
    def setOutputDir(dir: String) = dirPath = dir
    override def outputDir = dirPath
  }
  var runner: XmlRunner with Console with MockFileSystem with OutputDirSettable = _
  object simpleSpecRunner extends XmlRunner(sp1) with Console with MockFileSystem with MockOutput with OutputDirSettable
  object compositeSpecRunner extends XmlRunner(compositeSpec) with Console with MockFileSystem with MockOutput with OutputDirSettable
  object compositeSpec extends Specification { 
    "a composite spec" isSpecifiedBy(sp1, sp1)
  }
}
class xmlTest extends JUnit4(xmlRunnerSpec)

  object sp1 extends Specification {
    override def toString = name
    "the sus" should {
      "have one ok example" in {1 mustBe 1}
      "have one ko example" in {1 mustBe 2}
      "have an example with an exception" in {throw new Error("error message")}
      "have one sub-example" in { "a sub-example" in {1 mustBe 1}}
    }
  }
