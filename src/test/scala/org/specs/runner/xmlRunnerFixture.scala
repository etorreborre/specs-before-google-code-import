package org.specs.runner
import org.specs.specification._
import org.specs.util._
import scala.xml._


trait RunnerFixture extends LiteralSpecification with RunnerTestData {
  def createSimpleSpecRunner = runner = simpleSpecRunner
  def executeCompositeSpecRunner = { runner = compositeSpecRunner; executeRunner }
  def executeRunner = { runner.reset; runner.reportSpec.shh }
  def runnerOutputDir = { runner.outputDir_= _ }
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
  import org.specs.specification._
  var path = Property("")
  var xml: Property[Elem] = Property(<p></p>).onToString(e => new PrettyPrinter(200, 2).format(e))
  var runner: XmlRunner with MockFileSystem = _
  object simpleSpecRunner extends XmlRunner(spec1) with MockFileSystem with MockOutput
  object spec1 extends Specification {
    override def toString = name
    "the sut" should {
      "have one ok example" in {1 mustBe 1}
      "have one ko example" in {1 mustBe 2}
      "have an example with an exception" in {throw new Error("error message")}
      "have one sub-example" in { "a sub-example" in {1 mustBe 1}}
    }
  }
  object compositeSpecRunner extends XmlRunner(compositeSpec) with MockFileSystem with MockOutput
  object compositeSpec extends Specification { 
    "a composite spec" isSpecifiedBy(spec1, spec1)
  }
}