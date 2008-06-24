package org.specs.specification
import org.specs.matcher._

object taggedSpec extends Specification {
  "A tagged object" should { doBefore(createTagged)
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
  var tagged = new Object with Tagged
  def createTagged = tagged = new Object with Tagged
  def beRejected = beAccepted.not
  def beAccepted = new Matcher[Tagged] {
    def apply(v: => Tagged) = (v.isAccepted, "the tag is accepted", "the tag isn't accepted")
  }
}
class taggedSpecTest extends org.specs.runner.JUnit4(taggedSpec)