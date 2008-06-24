package org.specs.matcher
import org.specs.matcher.MatcherUtils._

/**
 * The <code>PatternMatchers</code> trait provides matchers which allow to use pattern matching
 * to match expressions
 */
trait PatternMatchers {
  
  /**
   * Matches if the value <code>v</code> is like the pattern <code> { case expression => boolean }</code><p>
   * It uses the fact that we can use the following syntax to map Options:<ul> 
   *  <li><code> myOption.map { case expression => boolean }</code><p></ul>
   * In that case, the pattern parameter would be <code>{ case expression => boolean }</code>, a function of type <code>Any => Boolean</code><p>
   * The <code>Sugar</code> object can be used to get a shorter expression by having the <code>ok</code> alias for <code>true</code>:
   *  <ul><li> <code>List(1, 2) must beLike { case x::y::Nil => ok }</code></ul>
   * 
   * @return false if there is no match
   * @return the value inside the mapped option if there is a match, which should be <code>true</code>
   * @param pattern a case expression
   */  
  def beLike(pattern: => (Any => Boolean)) = new Matcher[Any](){
     def apply(v: => Any) = {
       val value = v
      (
       try {
        if (value == null)  false  else  Some(value).map(pattern).get 
      } catch { case e: scala.MatchError => false }, 
      q(value) + " matches the given pattern", 
      q(value) + " doesn't match the expected pattern")
     }
  }
  
  /**
   * Matches if the value <code>v</code> is None
   */
  def beNone[T] = new Matcher[Option[T]](){
     def apply(v: => Option[T]) = { 
       val value = v
       val none: Option[T] = None
       ( 
       value match { 
        case n if (n == none) => true
        case _ => false 
      }, 
      q(value) + " is None", 
      q(value) + " is not None")
    }
  }
  
  /**
   * Matches if a is None when v is None and a is not None when v is not None
   */   
  def beAlsoNone[T](a: =>Option[T]) = new Matcher[Option[T]](){
    def apply(v: =>Option[T]) = {
      val x = a; 
      val y = v; 
      (x == None && y == None || x != None && y != None, "both values are None", if (x == None) q(y) + " is not None" else q(x) + " is not None")
    } 
  }  


  /**
   * Matches if the value <code>v</code> is Some(x)
   */
  def beSome[T] = new CaseMatcher[T](){
     def someApply(v: => Option[T]) = {
       val value = v
       ( 
       value match {
          case Some(x) => true 
          case _ => false
        },
        q(value) + " is Some(x)", 
        q(value) + " is not Some(x)")
     }
   }

  /**
   * Alias for beSome[Any]
   */
  def beSomething = beSome[Any]


  /**
   * The CaseMatcher class allow to verify expressions such as:<br>
   * <code>Some(x) must beSome[String].which(_.startWith("abc"))</code>
   */
  abstract class CaseMatcher[T] extends Matcher[Option[T]] {
    private var whichFunction: Option[T => Boolean] = None
    def which(g: T => Boolean) = {
      whichFunction = Some(g) 
      this
    }
    def someApply(value: => Option[T]): (Boolean, String, String)
    override def apply(a: => Option[T]) = 
      if (whichFunction == Some(null))
        (false, "the 'which' property is a not a null function", "the 'which' property is a null function")
      else
        whichFunction match {
          case None => someApply(a)
          case Some(g) => ( a match {
                          case Some(x) => g(x) 
                          case _ => false
                        },
                        "there is a Some(x) verifying the given property", 
                        "there is no Some(x) verifying the given property")
    }
  }
}
/**
 * Companion object for PatternMatchers
 */
object PatternMatchers extends PatternMatchers
