package org.specs

/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions,<br>
 * The name reminds that it must be used with caution
 */
object Sugar extends Sugar

/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions,<br>
 * The name reminds that it must be used with caution
 */
trait Sugar extends Products {
  
  /** alias for the value true. Allows to write <code> myObject.status mustBe ok </code>*/
  val ok = true
  
  /** alias for the value false. Allows to write <code> myObject.status mustBe ko </code>*/
  val ko = false

  /** 
   * This implicit definition allows to write short loops, ruby-style:<br/>
   * <code> 3.times { i => doThis() } </code>. 
   * Warning: an integer variable i must be declared otherwise there will be a runtime exception
   */
  implicit def integerToRange(n: Int): RangeInt = new RangeInt(n)
  case class RangeInt(n: Int) { 
    def times[T](f: (Int) => T)  = for (i <- 1 to n) f(i) 
  }
  
  /** 
   * This implicit definition allows to print any object to the console with:<br/>
   * <code>myObject.pln</code> or <code>myObject.println</code>  
   */
  implicit def anyPrintable[T](a: T) = new Printable(a)
  class Printable[T](a: T){
      def println = Console.println(a)
      def pln = println

      /** print and pass: print the value and return it */ 
      def pp = {Console.println(a); a}
  }
}

