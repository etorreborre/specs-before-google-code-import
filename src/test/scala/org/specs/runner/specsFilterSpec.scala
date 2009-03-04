package org.specs.runner
import org.specs.Specification

object specsFilterSpec extends Specification {
  "a specs filter" should {
    "filter the SUS of the specification according to a regular expression" in {
      object spec extends Specification {
        "this sus is sus1 and it" should { "have one example" in {} }
        "this sus is sus2 and it" should { "have one example" in {} }
      }
      val systems = filter(List(spec), ".*sus2.*", ".*").first.systems
      systems.size must_== 1
      systems.first.description must beMatching("sus2")
    }
    "filter out sus with no examples" in {
      object spec extends Specification {
        "this sus is sus1 and it" should {
          "have one example ex1" in {}
          "have one example ex2" in {}
        }
        "this sus is sus2 and it" should { "have one example ex1" in {} }
      }
      filter(List(spec), ".*", ".*ex2").first.systems.size must_== 1
    }
    "filter the examples of the specification according to a regular expression" in {
      object spec extends Specification {
        "this sus is sus1 and it" should {
          "have one example ex1" in {}
          "have one example ex2" in {}
        }
        "this sus is sus2 and it" should { "have one example ex1" in {} }
      }
      val systems = filter(List(spec), ".*", ".*ex1").first.systems
      systems.size must_== 2
      val s1 = systems(0)
      s1.examples.size must_== 1
      s1.examples.first.description must beMatching("ex1")

      val s2 = systems(1)
      s2.examples.size must_== 1
      s2.examples.first.description must beMatching("ex1")
    }
  }
  def filter(specifications: List[Specification], susToFilter: String, examplesToFilter: String) =  new SpecsFilter {
    val specs = specifications
    override def susFilterPattern = susToFilter
    override def exampleFilterPattern = examplesToFilter
  }.filter(specifications)
}
import org.specs.runner._
class specsFilterSpecTest extends JUnit4(specsFilterSpec)