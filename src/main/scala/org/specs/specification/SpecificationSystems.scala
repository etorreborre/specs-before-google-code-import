package org.specs.specification

/**
 * This trait abstracts the building and storing of the systems of a Specification.
 */
trait SpecificationSystems { this: BaseSpecification =>
  /** list of systems under test */
  var systems : List[Sus] = Nil

  /**
   * implicit definition allowing to declare a new system under test described by a string <code>desc</code><br>
   * Usage: <code>"my system under test" should {}</code><br>
   * Alternatively, it could be created with:
   * <code>specify("my system under test").should {}</code>
   */
  implicit def specifySus(desc: String): SpecifiedSus = {
    new SpecifiedSus(addSus(new Sus(desc, this)))
  }
  def specify(desc: String): Sus = specifySus(desc).sus
  /** 
   * this class is used instead of using the Sus directly in order to make sure that only "should" and "can" are added
   * to Strings
   */
  class SpecifiedSus(val sus: Sus) {
    def should(a: =>Any) = sus.should(a)
    def should(a: =>Unit) = sus.should(a)
    def can(a: =>Any) = sus.should(a)
    def can(a: =>Unit) = sus.should(a)
  }
  /**
   * specifies an anonymous Sus included in this specification
   */
  def specify: Sus = {
    addSus(new Sus(this))
  }
  /** 
   * add a new Sus to this specification
   */
  private[specs] def addSus(sus: Sus): Sus = {
    addChild(sus)
    systems = systems ::: List(sus)
    if (this.isSequential) sus.setSequential
    sus
  }
  /**
   * add a textual complement to the sus verb.
   * For example, it is possible to declare:
   * <code>"the system" should provide {...}</code>
   * if the following function is declared:
   * <code>def provide = addToSusVerb("provide")</code>
   */
  def addToSusVerb(complement: String) = new Function1[Example, Example] {
    def apply(e: Example) = { 
      current match { 
        case Some(sus: Sus) => sus.verb += " " + complement 
        case _ => 
      }
      e
    }
  }
}
