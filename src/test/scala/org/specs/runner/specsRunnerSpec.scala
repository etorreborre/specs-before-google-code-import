package org.specs.runner

import org.specs._
import org.specs.util._
import scala.collection.mutable.Queue
import org.specs.io.mock.MockOutput
import org.specs.runner._

class specsRunnerTest extends JUnit3(specsRunnerSpec)
object specsRunnerSpec extends Specification with TestRunner {
  "A specs file runner" should {doBefore { runner.messages.clear }
    
    "execute a specification contained in a file" in { 
      runTheFileWithClassName("org.specs.samples.sampleSpec1$")
      messages mustHaveMatch "example"
    }
    "execute 2 specifications contained in a directory" in { 
      runTheFileWithClassName("org.specs.samples.sampleSpec1$", "org.specs.samples.sampleSpec2$")
      messages mustHaveMatch "specification1"
      messages mustHaveMatch "specification2"
    }
  }
}

trait MockSpecsFinder extends SpecsFinder {
  var classNames: List[String] = Nil
  override def specificationNames(filesPath: String, pattern: String) = classNames
}
trait TestRunner {
  object runner extends SpecsFileRunner("", ".*") with MockSpecsFinder with MockOutput
  def runTheFileWithClassName(classNames: String*) = {
    runner.classNames = classNames.toList 
    runner.reportSpecs
  }
  def messages = runner.messages
}
object AllSpecsFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Spec") 
object AllUnitFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Unit") 
object AllFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*.*") 


