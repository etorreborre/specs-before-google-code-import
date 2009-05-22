package org.specs.specification
import org.specs.util.ReinitProperty
import scala.collection.mutable.ListBuffer
import org.specs.util.ReinitProperty

trait ContextProperties extends BaseSpecification { this: BeforeAfter =>
  private val exampleProperties = new ListBuffer[ReinitProperty[_]]
  private val susProperties = new ListBuffer[ReinitProperty[_]]
  def exampleProp[T](t: T) = { 
    val p = ReinitProperty(t)
    exampleProperties.append(p)
    p
  }
  def susProp[T](t: T) = { 
    val p = ReinitProperty(t)
    susProperties.append(p)
    p
  }
  override def addSus(sus: Sus): Sus = {
    exampleProperties.foreach { p => stackBeforeActions(sus, () => p.reinit) }
    susProperties.foreach { p => stackFirstActions(sus, p.reinit) }
    super.addSus(sus)
  }

}
