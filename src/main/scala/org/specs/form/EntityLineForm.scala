package org.specs.form

class EntityLineForm[T](var entity: Option[T]) extends LineForm {
  def this() = this(None)
  /** add a new LineProp to that line */
  def prop[S](s: String, f:(T => S)) = {
    lazy val actual: Option[S] = entity.map(f(_))
    val p = new LineProp(label, None, actual, Some(MatcherConstraint((m:org.specs.matcher.Matcher[S]) => actual.map(_ must m))))
    lineProperties.append(p)
    add(p)
    p
  }
  def entityIs(a: T): this.type = entityIs(Some(a))
  def entityIs(a: Option[T]): this.type = { entity = a; this }
}
