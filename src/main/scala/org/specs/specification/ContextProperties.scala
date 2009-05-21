package org.specs.specification
import scala.collection.mutable.ListBuffer
import org.specs.util.ReinitProperty

trait ContextProperties { this: BeforeAfter with BaseSpecification =>
  private[specification] val exampleProperties: ListBuffer[ReinitProperty[_]] = new ListBuffer[ReinitProperty[_]] 
  private[specification] val susProperties: ListBuffer[ReinitProperty[_]] = new ListBuffer[ReinitProperty[_]] 
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
  doBeforeSpec(systems.foreach { sus => stackFirstActions(sus, susProperties.foreach(_.reinit)) })
  doBefore(exampleProperties.foreach(_.reinit))
}
