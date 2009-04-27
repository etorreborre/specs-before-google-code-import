package org.specs.form
import org.specs.util.Property

class EntityLineProp[E, T](override val label: String,
                    expectedValue: Property[T],
                    function: E => T, 
                    val entity: Property[E], constraint: Option[MatcherConstraint[T]]) extends 
  LineProp[T](label, expectedValue, entity.map(function(_)), constraint) {

  override def copy = {
    val e = Property[E]()
    new EntityLineProp[E, T](label, expectedValue, function, e, 
                                     constraint.map(c => new MatcherConstraint(e.map(function(_)).optionalValue, c.executor))).asInstanceOf[this.type]
  }
}
object EntityLineProp {
  def apply[E, T](label: String, expected: =>T, function: E => T, entity: => E, constraint: MatcherConstraint[T]) = {
    new EntityLineProp(label, Property(expected), function, Property(entity), Some(constraint))
  }
  
}

