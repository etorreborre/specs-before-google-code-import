package org.specs.specification
import org.specs.matcher._

object taggedSpec extends Specification {
  "A tagged object" should { createTagged.before
    "be accepted if there is no tag added" in {
      tagged must beAccepted
    }
    "be accepted if there is no tag added and only some tags are accepted" in {
      tagged.accept("a") must beRejected
    }
    "be included if it accepts some tags it owns" in {
      tagged.addTag("a").accept("a") must beAccepted
    }
    "be accepted if a tag is not rejected" in {
      tagged.addTag("a") must beAccepted
    }
    "be rejected if a tag is rejected" in {
      tagged.addTag("a").reject("a") must beRejected
    }
    "be rejected if a tag is not in the accepted tags" in {
      tagged.addTag("a").accept("b") must beRejected
    }
    "be rejected if a tag is in the accepted tags but also in the rejected tags" in {
      tagged.addTag("a").accept("a").reject("a") must beRejected
    }
    "be accepted if a tag is in the accepted tags but not in the rejected tags" in {
      tagged.addTag("a").accept("a").reject("b") must beAccepted
    }
    "be taggable with another tagged object" in {
      val other = (new Object with Tagged).addTag("a").accept("a").reject("b")
      
      tagged.tagWith(other)
      tagged.tags must_== other.tags
      tagged.accepted must_== other.accepted
      tagged.rejected must_== other.rejected
    }
  }
  "A Tag object" should {
    "match another tag with the same name" in {
      Tag("hello") must beMatching(Tag("hello"))
    }
    "match another tag with a regex" in {
      Tag("hello") must beMatching(Tag("h.*"))
    }
    "match another tag with a name if it is itself a regex" in {
      Tag("h.*") must beMatching(Tag("hello"))
    }
    "match another tag with a name if both are regexes" in {
      Tag("h.*") must beMatching(Tag("h.*"))
    }
    "not match another tag even if its regex is not formed ok" in {
      Tag("h.*") must beMatching(Tag("h][")).not
    }
    "not match another tag even if it is null" in {
      Tag("h.*") must beMatching(Tag(null)).not
    }
    "not match another tag even if it is itself null" in {
      Tag(null) must beMatching(Tag("sdf")).not
    }
  }
  "A tagged object with subcomponents" should { createTaggedTree.before
    "propagate its tags to the subcomponents" in {
      taggedTree.tag("1")
      taggedTree.taggedComponents.first.tags must haveSameElementsAs(List(Tag("1")))
    }
    "clear the subcomponents tags when clearing its own" in {
      taggedTree.tag("1")
      taggedTree.clearTags
      taggedTree.taggedComponents.first.tags must beEmpty
    }
    "be able to accept all tags if some tags were previously rejected" in {
      taggedTree.tag("1")
      taggedTree.acceptTag("1")
      taggedTree.acceptAnyTag
      taggedTree.taggedComponents.first.accepted must beEmpty
      taggedTree.taggedComponents.first.rejected must beEmpty
    }
  }
  var tagged = new Object with Tagged
  var taggedTree = new Object with Tagged
  def createTagged = tagged = new Object with Tagged
  def createTaggedTree = {
    val child = new Object with Tagged
    taggedTree = new Object with Tagged { override def taggedComponents = List(child) }
  }
  def beRejected = beAccepted.not
  def beAccepted = new Matcher[Tagged] {
    def apply(v: => Tagged) = (v.isAccepted, "the tag is accepted", "the tag isn't accepted")
  }
  def beMatching(other: Tag) = new Matcher[Tag] {
    def apply(v: => Tag) = (v.matches(other), v + " matches " + other, v + " doesn't match " + other)
  }
}
class taggedSpecTest extends org.specs.runner.JUnit4(taggedSpec)