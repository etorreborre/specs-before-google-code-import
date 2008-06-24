package org.specs.matcher

import org.specs.specification._
import org.specs.matcher.MatcherUtils.{q, matches}
import org.specs.matcher.PatternMatchers._
import org.specs.ExtendedThrowable._
import org.specs.util.EditDistance._
import org.specs.collection.ExtendedIterable._
import scala.collection.immutable.{Set => Removed}
import scala.collection.Set

object AnyMatchers extends AnyMatchers
/**
 * The <code>AnyMatchers</code> trait provides matchers which are applicable to any scala reference or value
 */
trait AnyMatchers {

  /**
   * Matches if (a eq b)
   */   
  def be(a: =>Any) = new Matcher[Any](){
    def apply(v: =>Any) = {
      val (x, y) = (a, v) 
      (x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef], q(y) + " is the same as " + q(x), q(y) + " is not the same as " + q(x))} 
  } 
  def notBe(a: =>Any) = be(a).not

  /**
   * Matches if (a == b)
   */   
  def beEqual[T](a: =>T) = new Matcher[T](){
    def apply(v: =>T) = {
      val (x, y) = (a, v) 
      (x == y, q(y) + " is equal to " + q(x), q(y) + " is not equal to " + q(x))
    } 
  } 

  /**
   * Matches if (a != b)
   */   
  def beDifferent[T](a: =>T) = beEqual[T](a).not

  /**
   * Matches if (a == b)
   */   
  def is_==(a: =>Any)(implicit d: Detailed) = new Matcher[Any](){ 
    def apply(v: =>Any) = {
      val (x, y) = (a, v)
      import org.specs.Products._
      val failureMessage = d match {
        case full: fullDetails => EditMatrix(q(y), q(x)).showDistance(full.separators).toList.mkString(" is not equal to ")
        case no: noDetails => q(y) + " is not equal to " + q(x)
      }
      ((x == y), q(y) + " is equal to " + q(x), failureMessage)
    }
  }

  /**
   * Alias of is_==
   */   
  def be_==(a: =>Any)(implicit d: Detailed) = is_==(a)(d)

  /**
   * Matches if (a neq b)
   */  
  def notEq(a: =>Any) = be(a).not 

  /**
   * Matches if (a != b)
   */   
  def is_!=(a: =>Any)(implicit d: Detailed) = (is_==(a)(d)).not 
  
  /**
   * Matches if (a != b)
   */   
  def be_!=(a: =>Any)(implicit d: Detailed) = (is_==(a)(d)).not 

  /**
   * Matches if b is null
   */   
  def beNull[T] = new Matcher[T](){
    def apply(v: =>T) = { val b = v; (b == null, "the value is null", q(b) + " is not null") } 
  }  

  /**
   * Matches if a is null when v is null and a is not null when v is not null
   */   
  def beAlsoNull[T](a: =>T) = new Matcher[T](){
    def apply(v: =>T) = {
      val x = a; 
      val y = v; 
      (x == null && y == null || x != null && y != null, "both values are null", if (x == null) q(y) + " is not null" else q(x) + " is not null")
    } 
  }  

  /**
   * Matches if b is not null
   */   
  def notBeNull[T] = beNull[T].not

  /**
   * Matches if b is true
   */   
  def beTrue = new Matcher[Boolean](){
    def apply(v: =>Boolean) = { val b = v; (b == true, "the value is true", "the value is false") } 
  }  

  /**
   * Matches if b is false
   */   
  def beFalse[Boolean] = new Matcher[Boolean](){
    def apply(v: =>Boolean) = { val b = v; (b == false, "the value is false", "the value is true") } 
  }  
  
  /**
   * Matches if iterable.exists(_ == a)
   */   
  def beIn[T <: AnyRef](iterable: =>Iterable[T]) = new Matcher[T](){
    def apply(v: => T) = {
      val (x, y) = (iterable, v) 
      (x.exists(_ == y), q(y) + " is in " + q(x), q(y) + " is not in " + q(x))
    }
  }

  /**
   * Matches if not(iterable.exists(_ == a))
   */   
  def notBeIn[T <: AnyRef](iterable: =>Iterable[T]) = beIn(iterable).not 

  /**
   * Matches if any object with an <code>isEmpty</code> method returns true: (Any {def isEmpty: Boolean}).isEmpty
   */   
  def beEmpty[S <: Any {def isEmpty: Boolean}] = new Matcher[S](){
    def apply(v: => S) = {val iterable = v; (iterable.isEmpty, iterable + " is empty", iterable + " is not empty")}
  }

  /**
   * Matches if not(beEmpty(a))
   */   
  def notBeEmpty[S <: Any {def isEmpty: Boolean}] = beEmpty[S].not

  /**
   * Alias of notBeEmpty
   */   
  def isNotEmpty[S <: Any {def isEmpty: Boolean}] = notBeEmpty[S] 

  /**
   * Alias of beEmpty
   */   
  def isEmpty[S <: Any {def isEmpty: Boolean}] = beEmpty[S]

  /**
   * Matches if the function f returns true
   */   
  def verify[T](f: T => Boolean): Matcher[T] = new Matcher[T](){
     def apply(v: => T) = {val x = v; (f(x), x + " verifies the property", x + " doesn't verify the expected property")}
  }
  
  /**
   * Matches if value is throwing an exception which is assignable from errorType.getClass
   * <br>Otherwise rethrow any other exception
   * <br>Usage: <code>value must throwA(new ExceptionType)</code>
   * <br>Advanced usage: <code>value must throwA(new ExceptionType).like {case ExceptionType(m) => m.startsWith("bad")}</code>
   */   
  def throwException[E <: Throwable](exception: =>E) = new ExceptionMatcher(exception)
    class ExceptionMatcher[E <: Throwable](exception: E) extends Matcher[Any] {
     def apply(value: => Any) = { 
       (isThrown(value, exception, (e => exception.getClass.isAssignableFrom(e.getClass))).isDefined, exception + " was thrown", exception + " should have been thrown")
     }
     def like[E <: Throwable](f: =>(Any => Boolean)) = new Matcher[Any](){
       def apply(v: => Any) = {
         val thrown = isThrown(v, exception, (e => exception.getClass.isAssignableFrom(e.getClass)))
         if (!thrown.isDefined)
           (false, exception + " was thrown", exception + " should have been thrown")
         else 
           beLike(f)(thrown.get)
       }
     }
   }  
  /**
   * Alias for throwException
   */   
  def throwA[E <: Throwable](e: =>E) = throwException[E](e)

  /**
   * Alias for throwException
   */   
  def throwAn[E <: Throwable](e: =>E) = throwException[E](e)

  /**
   * Matches if the thrown exception is == to e
   */   
  def throwThis[E <: Throwable](exception: =>E) = new Matcher[Any](){
    def apply(value: => Any) = { 
        (isThrown(value, exception, (e => e == exception)).isDefined, exception + " was thrown", exception + " should have been thrown")
    }
  }
  
  /**
   * Matches if v.getClass == c
   */   
  def haveClass[T](c: Class[T]) = new Matcher[Any](){
    def apply(v: =>Any) = {
      val x: Any = v
      val xClass = x.asInstanceOf[java.lang.Object].getClass
      (xClass == c, q(x) + " has class" + q(c.getName), q(x) + " doesn't have class " + q(c.getName) + " but " + q(xClass.getName))
    } 
  } 

  /**
   * Matches if v.getClass != c
   */   
  def notHaveClass[T](c: Class[T]) = haveClass(c).not

  /**
   * Matches if v.isAssignableFrom(c)
   */   
  def beAssignableFrom[T](c: Class[T]) = new Matcher[Class[_]](){
    def apply(v: =>Class[_]) = {
      val x: Class[_] = v
      (x.isAssignableFrom(c), q(x.getName) + " is assignable from " + q(c.getName), q(x.getName) + " is not assignable from " + q(c.getName))
    } 
  } 

  /**
   * Matches if v.isAssignableFrom(c)
   */   
  def notBeAssignableFrom[T](c: Class[T]) = beAssignableFrom(c).not

  /**
   * Matches if c.isAssignableFrom(v)
   */   
  def haveSuperClass[T](c: Class[T]) = new Matcher[Any](){
    def apply(v: =>Any) = {
      val x: Any = v
      val xClass = x.asInstanceOf[java.lang.Object].getClass
      (c.isAssignableFrom(xClass), q(x) + " has super class" + q(c.getName), q(x) + " doesn't have super class " + q(c.getName))
    } 
  } 

  /**
   * Matches if c.isAssignableFrom(v)
   */   
  def notHaveSuperClass[T](c: Class[T]) = haveSuperClass(c).not

  /**
   * @returns an Option with the expected exception if it satisfies function <code>f</code>
   * <br>rethrows the exception otherwise
   */   
  private def isThrown[E <: Throwable](value: => Any, expected: E, f: (Throwable => Boolean)) = { 
    getException(value) match {
      case None => None
      case Some(e)  => if (f(e))
                         Some(e)
                       else
                         throwFailure(e, expected + " should have been thrown. Got: " + e)
    }
  }
  /** evaluates a value and return any exception that is thrown */
  private def getException[E <: Throwable](value: => Any): Option[Throwable] = {
    try { value } 
    catch { case e => { return Some(e)} }
    return None
  }

  /**
   * Creates a FailureException corresponding to a thrown exception.
   * <br>Sets the stacktrace of the <code>FailureException</code> so that it starts with the code line where the original exception
   * was thrown
   * @param e original exception 
   * @param failureMessage exception message 
   */
  def throwFailure(e: =>Throwable, failureMessage: =>String) = {
    val failure = FailureException(failureMessage) 
    failure.setStackTrace((e.getStackTrace.toList.dropWhile {x: StackTraceElement => x.toString.matches("AnyMatchers") || x.toString.matches("Assert")}).toArray)
    throw failure
  }
  
  /**
   * Adds functionalities to functions returning matchers so that they can be combined before taking a value and 
   * returning actual matchers
   */
  implicit def toMatcher[S, T](f: S => Matcher[T]) = new ToMatcher(f)
  implicit def toMatcher2[T](f: T => Matcher[T]) = new ToMatcher2(f)

  /**
   * The <code>ToMatcher</code> class allows to combine functions returning matchers, or a function returning a matcher and a matcher.<p>
   * For example:<code>((beEqual(_:Int)) or (be_>(_:Int)))(3) </code> 
   */
  class ToMatcher[S, T](f: S => Matcher[T]) {
    /**
     * @return a function which will return the and of 2 matchers 
     */
    def and(m: =>Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) and m 
    }
    /**
     * @return a function which will return the or of 2 matchers 
     */
    def or(m: =>Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) or m 
    }
    /**
     * @return a function which will return the xor of 2 matchers 
     */
    def xor(m: Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) xor m 
    }
    /**
     * @return a function which will return the negation of a matcher 
     */
    def not = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s).not 
    }
    /**
     * @return a function which will return the and of 2 matchers 
     */
    def and(m: S => Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) and m(s) 
    }
    /**
     * @return a function which will return the or of 2 matchers 
     */
    def or(m: S => Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) or m(s) 
    }
    /**
     * @return a function which will return the xor of 2 matchers 
     */
    def xor(m: S => Matcher[T]) = new Function1[S, Matcher[T]] {
      def apply(s: S) = f(s) xor m(s) 
    }

    /**
     * @return a function which will return the composition of a matcher and a function 
     */
    def ^^[A](g: A => S) = new Function1[A, Matcher[T]] {
      def apply(a: A) = f(g(a)) 
    }

    /**
     * @return a function which will return a matcher checking a sequence of objects 
     */
    def toSeq = new Function1[Seq[S], Matcher[Seq[T]]] {
      def apply(s: Seq[S]) = new SeqMatcher(s, f) 
    }

    /**
     * @return a function which will return a matcher checking a set of objects 
     */
    def toSet = new Function1[Set[S], Matcher[Set[T]]] {
      def apply(s: Set[S]) = new SetMatcher(s, f) 
    }
  }
  class ToMatcher2[T](f: T => Matcher[T]) {
    /**
     * @return a function which will return the composition of a matcher and a function 
     */
    def ^^^[A](g: A => T) = {
      (a: A) => new Matcher[A] {
        def apply(b: =>A) = {
          f(g(a)).apply(g(b))
        }
      }
    } 
  }

  /**
   * The <code>SeqMatcher</code> class is a matcher matching a sequence of objects with a matcher returned by a function.<p>
   * Usage:<code>List(1, 2, 3) must ((beEqual(_:Int)).toSeq)(List(1, 2, 3)) </code> 
   */
  class SeqMatcher[S, T](s: Seq[S], f: S => Matcher[T]) extends Matcher[Seq[T]] {
    def apply(t: => Seq[T]) = {
      val bothSequences = t.toList zip s.toList
      val results = bothSequences map { st => val (t1, s1) = st 
                                        f(s1).apply(t1) }
      (results.map(_._1).reduceLeft(_ && _), results.filter(_._1).map(_._2).mkString("; "), 
                                             results.filter(!_._1).map(_._3).mkString("; "))
    }
  }

  /**
   * The <code>SetMatcher</code> class is a matcher matching a set of objects with a matcher returned by a function.<p>
   * Usage:<code>List(1, 2, 3) must ((beEqual(_:Int)).toSet)(List(2, 1, 3)) </code> 
   */
  class SetMatcher[S, T](s: Set[S], f: S => Matcher[T]) extends Matcher[Set[T]] {
    def apply(t: => Set[T]) = {
      val setToTest = t
      if (s.size != setToTest.size)
        (false, "the sets contain the same number of elements", 
                 q(setToTest) + " contains " + setToTest.size + " elements while " + q(s) + " contains " + s.size + " elements")
      else {
        val results = setToTest.map {(element: T) => 
          s.find { (otherElement:S) => f(otherElement).apply(element).success } match {
            case None => (false, "all matches", "no match for element " + q(element))
            case Some(x) => (true, q(element) + " matches with " + x, "no match for element " + q(element))
          }
        }
        (results.map(_._1).reduceLeft(_ && _), results.filter(_._1).map(_._2).mkString("; "), 
                                               results.filter(!_._1).map(_._3).mkString("; "))
      }
    }
  }
}
