package org.specs.specification

/**
 * This trait adds the possibility to declare an included specification as "linked" in order to 
 * control its reporting in a separate file for example.
 */
trait LinkedSpecification { this: BaseSpecification => 
  /** storing the parent links for this specification */
  private var parentLinks = List[LinkedSpecification]()
  /** add a new parent link to this specification */
  private[specs] def addParent(s: LinkedSpecification): this.type = { parentLinks = s :: parentLinks; this }
  /** @return true if this specification has s as one of its parents */
  private[specs] def hasParent(s: LinkedSpecification): Boolean = parentLinks.contains(s)
  /** link this specification to a subordinate one */
  def linkTo(subSpec: Specification): this.type = {
    if (!contains(subSpec)) include(subSpec)
    subSpec.addParent(this)
    this
  }
  /** 
   * partitions the subspecifications in a pair where the first member is the list of linked specifications, 
   * and the second member is the unlinked ones
   */
  def partitionLinkedSpecifications: (List[Specification], List[Specification]) = {
    this.subSpecifications.partition(_.hasParent(this))  
  }
  /** @return the linked specifications */
  def linkedSpecifications = this.partitionLinkedSpecifications._1
  /** @return the unlinked specifications */
  def unlinkedSpecifications = this.partitionLinkedSpecifications._2 
}
