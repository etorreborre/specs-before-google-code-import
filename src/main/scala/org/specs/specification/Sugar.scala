package org.specs
import org.specs.io._
/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions,<br>
 * The name reminds that it must be used with caution
 */
object Sugar extends Sugar with ConsoleOutput

/**
 * Synctactic sugar for specifications. Since it makes heavy uses of implicit definitions,<br>
 * The name reminds that it must be used with caution
 */
trait Sugar extends Products with Output { outer =>
  
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
      def println = outer.println(a)
      def pln = println

      /** print and pass: print the value and return it */ 
      def pp = { outer.println(a); a }
  }
  /** 
   * This implicit definition allows to print any iterable to the console with:<br/>
   * <code>myIterable.printEach</code>. It also returns the iterable for better method insertion  
   */
  implicit def iterableToPrintable[T](a: Iterable[T]) = new PrintableIterable(a)
  class PrintableIterable[T](a: Iterable[T]){
      /** print each element and return the iterable */ 
      def printEach = { a foreach(outer.println(_)); a }
      /** print each element forcing a given output object */ 
      def printEach(output: { def println(x: Any) }) = { a foreach(output.println(_)); a }
  }
}

