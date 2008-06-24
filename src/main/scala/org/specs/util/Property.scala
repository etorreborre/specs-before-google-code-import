package org.specs.util

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
