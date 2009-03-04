package org.specs.runner

import org.specs._
import org.specs.util._
import scala.collection.mutable.Queue
import org.specs.io.mock.MockOutput
import org.specs.runner._
import org.specs.specification._

object specsRunnerSpec extends Specification with TestRunner {
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
    "filter the SUS of the specification according to a regular expression" in {
      object runner extends SpecsFileRunner("", ".*", ".*sus2", ".*")
      object spec extends Specification {
        "this sus is sus1 and it" should { "have one example" in {} }
        "this sus is sus2 and it" should { "have one example" in {} }
      }
      val systems = runner.filter(List(spec)).first.systems
      systems.size must_== 1
      systems.first.description must beMatching("sus2")
    }
    "filter out sus with no examples" in {
      object runner extends SpecsFileRunner("", ".*", ".*", ".*ex2")
      object spec extends Specification {
        "this sus is sus1 and it" should {
          "have one example ex1" in {}
          "have one example ex2" in {}
        }
        "this sus is sus2 and it" should { "have one example ex1" in {} }
      }
      runner.filter(List(spec)).first.systems.size must_== 1
    }
    "filter the examples of the specification according to a regular expression" in {
      object runner extends SpecsFileRunner("", ".*", ".*", ".*ex1")
      object spec extends Specification {
        "this sus is sus1 and it" should {
          "have one example ex1" in {}
          "have one example ex2" in {}
        }
        "this sus is sus2 and it" should { "have one example ex1" in {} }
      }
      val systems = runner.filter(List(spec)).first.systems
      systems.size must_== 2
      val s1 = systems(0)
      s1.examples.size must_== 1
      s1.examples.first.description must beMatching("ex1")

      val s2 = systems(1)
      s2.examples.size must_== 1
      s2.examples.first.description must beMatching("ex1")
    }
  }
}
class specsRunnerTest extends JUnit4(specsRunnerSpec)

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


