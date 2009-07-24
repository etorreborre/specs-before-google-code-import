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
  implicit def specify(desc: String): Sus = {
    addSus(new Sus(desc, this))
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
