package org.specs.specification
import org.specs.util.ReinitProperty
import scala.collection.mutable.ListBuffer
import org.specs.util.ReinitProperty

trait ContextProperties extends SpecificationSystems { this: BeforeAfter with BaseSpecification =>
  implicit def toExamplePropertyBeforeExample[T](t: =>T) = new ExamplePropertyBeforeExample(t)
  class ExamplePropertyBeforeExample[T](t: =>T) {
    def beforeEx = exampleProp(t)
    def beforeExample = beforeEx
  }
  implicit def toSusPropertyBeforeExample[T](t: =>T) = new SusPropertyBeforeSus(t)
  class SusPropertyBeforeSus[T](t: =>T) {
    def beforeSus = susProp(t)
  }
  private val exampleProperties = new ListBuffer[ReinitProperty[_]]
  private val susProperties = new ListBuffer[ReinitProperty[_]]
  def exampleProp[T](t: T) = { 
    val p = ReinitProperty(t)
    if (!systems.isEmpty)
      stackBeforeActions(currentSus, () => p.reinit)
    else
      exampleProperties.append(p)
    p
  }
  def susProp[T](t: T) = { 
    val p = ReinitProperty(t)
    if (!systems.isEmpty)
      stackFirstActions(currentSus, p.reinit)
    else
      susProperties.append(p)
    p
  }
  override def addSus(sus: Sus): Sus = {
    exampleProperties.foreach { p => stackBeforeActions(sus, () => p.reinit) }
    susProperties.foreach { p => stackFirstActions(sus, p.reinit) }
    super.addSus(sus)
  }
}
