package org.specs.runner

import org.specs._
import org.specs.util._
import scala.collection.mutable.Queue
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs.specification._

class specsRunnerSpec extends Specification with TestRunner with JUnit {
  "A specs file runner" should { runner.messages.clear.before

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
  "A specs file runner" should {
  }
}
trait MockSpecsFinder extends SpecsFinder {
  var classNames: List[String] = Nil
  override def specificationNames(filesPath: String, pattern: String) = classNames
}

trait TestRunner {
  class SpecsRunner extends SpecsFileRunner("", ".*") with MockSpecsFinder with MockOutput
  var runner: SpecsRunner = new SpecsRunner
  def runTheFileWithClassName(classNames: String*) = {
    runner = new SpecsRunner
    runner.classNames = classNames.toList
    runner.reportSpecs
  }
  def messages = runner.messages
}

object AllSpecsFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Spec")
object AllUnitFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*Unit")
object AllFileRunner extends SpecsFileRunner("./src/test/scala/org/specs", "([^(?>all)].)*.*")


