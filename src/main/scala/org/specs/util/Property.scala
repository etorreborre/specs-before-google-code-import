package org.specs.util
import scala.xml._

/**
 * This class represent properties which can be updated and retrieved using customized getter and setter functions
 */
case class Property[T](init: T) {
  /**
   * raw value of the property
   */
  private var value: T = init

  /**
   * setter function used to set the property value. The default is the identity function
   */
  private var setter: T => T = identity[T]

  /**
   * getter function used to retrieve the property value. The default is the identity function
   */
  private var getter: T => T = identity[T]

  /**
   * display function used to print the property value. The default is the toString method
   */
  private var toStringer: T => String = (v: T) => v.toString

  /**
   * @returns a value using the getter function
   */
  def apply(): T = getter(value)
  def apply(newValue: T) = {value = setter(newValue); this}

  /**
   * updates the value using the setter function
   */
  def update(newValue: T) = {value = setter(newValue); this}

  /**
   * sets a new getter function
   */
  def onGet(newGetter: T => T) = {getter = newGetter; this}

  /**
   * sets a new setter function
   */
  def onSet(newSetter: T => T) = {setter = newSetter; this}

  /**
   * sets a new display function
   */
  def onToString(newToStringer: T => String) = {toStringer = newToStringer; this}

  /**
   * @returns the string value using the stringer function
   */
  override def toString = toStringer(value)
}
object Properties extends Properties
trait Properties {
    /**
   * This method is used setup a property value, in order to avoid repeting a string. For example: <pre>
   * The name of the person should be {"john" as personName in checkPersonName}
   * </pre>. 
   */
  implicit def anyToAs[T](a: T) = new AsProperty(a)
  implicit def propertyToValue[T](p: Property[T]):T = p()
  case class AsProperty[T](a: T) { 
    def as(p: Property[T]) = {p() = a; a }
    def apply(p: Property[T]) = {p() = a; a}
    def apply(f: T => Any)= {f(a); a }
    def as(f: T => Any)= {f(a); a }
  }

}
/**
 * This trait provides String properterties with alphabetical names.
 */
trait StringProperties { outer =>
  val a = Property[String]("")
  val b = Property[String]("")
  val c = Property[String]("")
  val d = Property[String]("")
  val e = Property[String]("")
  val f = Property[String]("")
  implicit def stringToAlpha(value: String) = StringToAlpha(value)
  case class StringToAlpha(value: String) {
    def a = { outer.a() = value; value }
    def b = { outer.b() = value; value }
    def c = { outer.c() = value; value }
    def d = { outer.d() = value; value }
    def e = { outer.e() = value; value }
    def f = { outer.f() = value; value }
  } 
}
/**
 * This trait provides Xml properterties with alphabetical names.
 */
trait XmlProperties { outer =>
  val xml  = Property[Elem](<e/>)
  val xml2 = Property[Elem](<e/>)
  val xml3 = Property[Elem](<e/>)
  val xml4 = Property[Elem](<e/>)
  val xml5 = Property[Elem](<e/>)
  val xml6 = Property[Elem](<e/>)
  implicit def elemToAlpha(value: Elem) = ElemToAlpha(value)
  case class ElemToAlpha(value: Elem) {
    def abc  = { outer.xml() = value; value }
    def xml2 = { outer.xml2() = value; value }
    def xml3 = { outer.xml3() = value; value }
    def xml4 = { outer.xml4() = value; value }
    def xml5 = { outer.xml5() = value; value }
    def xml6 = { outer.xml6() = value; value }
  } 
}
/**
 * This trait provides Int properterties with alphabetical names.
 */
trait IntProperties { outer =>
  val i = Property[Int](0)
  val j = Property[Int](0)
  val k = Property[Int](0)
  val l = Property[Int](0)
  val m = Property[Int](0)
  val n = Property[Int](0)
  implicit def intToAlpha(value: Int) = IntToAlpha(value)
  case class IntToAlpha(value: Int) {
    def i = { outer.i() = value; value }
    def j = { outer.j() = value; value }
    def k = { outer.k() = value; value }
    def l = { outer.l() = value; value }
    def m = { outer.m() = value; value }
    def n = { outer.n() = value; value }
  } 
}
/**
 * This trait provides Boolean properterties with alphabetical names.
 */
trait BooleanProperties { outer =>
  val o = Property[Boolean](true)
  val p = Property[Boolean](true)
  val q = Property[Boolean](true)
  val r = Property[Boolean](true)
  val s = Property[Boolean](true)
  val t = Property[Boolean](true)
  implicit def booleanToAlpha(value: Boolean) = BooleanToAlpha(value)
  case class BooleanToAlpha(value: Boolean) {
    def o = { outer.o() = value; value }
    def p = { outer.p() = value; value }
    def q = { outer.q() = value; value }
    def r = { outer.r() = value; value }
    def s = { outer.s() = value; value }
    def t = { outer.t() = value; value }
  } 
}
/**
 * This trait provides Double properterties with alphabetical names.
 */
trait DoubleProperties { outer =>
  val u = Property[Double](0.0)
  val v = Property[Double](0.0)
  val w = Property[Double](0.0)
  val x = Property[Double](0.0)
  val y = Property[Double](0.0)
  val z = Property[Double](0.0)
  implicit def doubleToAlpha(value: Double) = DoubleToAlpha(value)
  case class DoubleToAlpha(value: Double) {
    def u = { outer.u() = value; value }
    def v = { outer.v() = value; value }
    def w = { outer.w() = value; value }
    def x = { outer.x() = value; value }
    def y = { outer.w() = value; value }
    def z = { outer.z() = value; value }
  } 
}
/**
 * This trait one String property for a current value.
 */
trait CurrentProperty { outer => 
  val it = Property[String]("")
  implicit def stringToIt(s: String) = StringToIt(s)
  case class StringToIt(s: String) {
    def it = { outer.it() = s; s }
  } 
  
}
/**
 * This trait adds all properties.
 */
trait AllProperties extends StringProperties with IntProperties with DoubleProperties with BooleanProperties with CurrentProperty
object AllProperties extends AllProperties
