package org.specs.specification
import scala.collection.mutable.Queue

trait Tagged {
  
  val tags: Queue[Tag] = new Queue[Tag]
  val accepted: Queue[Tag] = new Queue[Tag]
  val rejected: Queue[Tag] = new Queue[Tag]
  def tag(t: String): this.type = addTag(t)
  def addTag(t: String): this.type = { tags.enqueue(Tag(t)); this }
  def addTags(t: String*): this.type = { t.foreach(addTag(_)); this }
  def accept(names: String*): this.type = { names.foreach { n => acceptTag(Tag(n))} ; this }
  def reject(names: String*): this.type = { names.foreach { n => rejectTag(Tag(n))} ; this }
  def acceptTag(t: Tag*): this.type = { accepted.enqueue(t:_*); this }
  def rejectTag(t: Tag*): this.type = { rejected.enqueue(t:_*); this }
  def isAccepted = {
    tags.isEmpty && accepted.isEmpty ||
    (accepted.isEmpty ||
    !accepted.isEmpty && tags.exists(t => accepted.exists(a => t matches a))) && 
    !tags.exists(t => rejected.exists(a => t.matches(a)))
  }
  implicit def stringToTag(s: String) = Tag(s)
  def tagSpec = "tags: " + tags.mkString(", ") + "  accepted: " + accepted.mkString(", ") + "  rejected: " + rejected.mkString(", ")
  def tagWith(other: Tagged): this.type = {
    this.addTags(other.tags.map(_.name):_*).
      accept(other.accepted.map(_.name):_*).
      reject(other.rejected.map(_.name):_*)
  }
}
case class Tag(name: String) {
  def matches(pattern: Tag) = name matches pattern.name
}
