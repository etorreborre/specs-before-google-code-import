package org.specs.matcher
import org.specs.matcher.MatcherUtils._
import org.specs.collection.ExtendedIterable._
import org.specs.matcher.AnyMatchers._
import org.specs.specification._
import org.specs.util.EditDistance._

/**
 * The <code>IterableMatchers</code> trait provides matchers which are applicable to Iterable objects
 */
trait IterableMatchers {
   
  /**
   * Matches if iterable.exists(_ == a)
   */   
  def contain[T](a: T) = new Matcher[Iterable[Any]](){ 
    def apply(v: => Iterable[Any]) = {val iterable = v; (iterable.exists(_ == a), d(iterable) + " contains " + q(a), d(iterable) + " doesn't contain " + q(a))} 
  }

  /**
   * Matches if not(iterable.exists(_ == a))
   */   
  def notContain[T](a: T) = contain(a).not 

  /**
   * Matches if all the elements of l are included in the actual iterable
   */   
  def containAll[T](l: Iterable[T])(implicit details: Detailed) = new Matcher[Iterable[T]]() { 
    def apply(v: => Iterable[T]) = {
      val iterable = v; 
      import org.specs.Products._
      val failureMessage = details match {
        case full: fullDetails => EditMatrix(d(iterable.mkString("\n")), q(l.mkString("\n"))).showDistance(full.separators).toList.mkString(" doesn't contain all of ")
        case no: noDetails => d(iterable) + " doesn't contain all of " + q(l)
      }
      (l.forall(x => iterable.exists(_ == x)), d(iterable) + " contains all of " + q(l), failureMessage)
    } 
  }

  /**
   * Alias for containAll.not
   */   
  def notContainAll[T](l: Iterable[T])(implicit details: Detailed) = containAll(l)(details).not

  /**
   * Matches if all the elements of l are included in the actual iterable in that order
   */   
  def containInOrder[T](l: Iterable[T])(implicit details: Detailed) = new Matcher[Iterable[T]](){ 
    def apply(v: => Iterable[T]) = {
      val iterable = v; 
      import org.specs.Products._
      val failureMessage = details match {
        case full: fullDetails => EditMatrix(d(iterable.mkString("\n")), q(l.mkString("\n"))).showDistance(full.separators).toList.mkString("", " doesn't contain all of ", " in order")
        case no: noDetails => d(iterable) + " doesn't contain all of " + q(l) + " in order"
      }
      (iterable.containsInOrder(l), d(iterable) + " contains all of " + q(l) + " in order", failureMessage)
    } 
  }
  
  /**
   * Matches if there is one element in the iterable verifying the <code>function</code> parameter: <code>(iterable.exists(function(_))</code>
   */   
  def have[T](function: T => Boolean) = new Matcher[Iterable[T]](){ 
    def apply(v: => Iterable[T]) = {val iterable = v; (iterable.exists{function(_)}, "at least one element verifies the property in " + d(iterable), "no element verifies the property in " + d(iterable))} 
  }
  /**
   * Matches if there is no element in the iterable verifying the <code>function</code> parameter: <code>!(iterable.exists(function(_))</code>
   */   
  def notHave[T](function: T => Boolean) = have(function).not 
  /**
   * Matches if there is one element in the iterable verifying the <code>function</code> parameter: <code>(iterable.exists(function(_))</code>
   * @deprecated  use have instead
   */   
  def exist[T](function: T => Boolean) = have(function)

  /**
   * Matches if there is no element in the iterable verifying the <code>function</code> parameter: <code>!(iterable.exists(function(_))</code>
   * @deprecated use notHave instead instead
   */   
  def notExist[T](function: T => Boolean) = notHave(function) 
  /**
   * Alias for existMatch
   * @deprecated: use containMatch instead
   */   
  def existMatch(pattern: String) = containMatch(pattern)
  /**
   * Matches if there is one element in the iterable[String] matching the <code>pattern</code> parameter: <code> iterable.exists(matches(pattern) _)</code>
   */   
  def containMatch(pattern: String) = new Matcher[Iterable[String]](){
    def apply(v: => Iterable[String]) = {val iterable = v; (iterable.exists( matches(pattern) _), "at least one element matches " + q(pattern) + " in " + d(iterable), "no element matches " + q(pattern) + " in " + d(iterable))}
  } 
  /**
   * Matches if not(existMatch(a))
   */   
  def notContainMatch(pattern: String) = containMatch(pattern).not 

  /**
   * Alias for notExistMatch
   * @deprecated: use notContainMatch instead
   */   
  def notExistMatch(pattern: String) = existMatch(pattern).not

  /**
   * @deprecated: use haveTheSameElementsAs instead
   */   
  def haveSameElementsAs[T](l: Iterable[T]) = haveTheSameElementsAs(l)
  /**
   * Matches if there l contains the same elements as the Iterable <code>iterable</code>.<br>
   * This verification does not consider the order of the elements but checks the iterables recursively
   */   
  def haveTheSameElementsAs[T](l: Iterable[T]) = new Matcher[Iterable[T]](){
    def apply(iterable: => Iterable[T]) = (
        l.sameElementsAs(iterable),
        d(l.toDeepString) + " has the same elements as " + q(iterable.toDeepString), 
        d(l.toDeepString) + " doesn't have the same elements as " + q(iterable.toDeepString))
  }

  /**
   * @deprecated: use beTheSameSeqAs instead
   */   
  def beSameSeqAs[T](s: =>Seq[T])(implicit d: Detailed) = beTheSameSeqAs(s)(d)
  /**
   * Matches if a sequence contains the same elements as s, using the equality (in the same order)
   */   
  def beTheSameSeqAs[T](s: =>Seq[T])(implicit d: Detailed) = (toMatcher(AnyMatchers.be_==(_:T)(d)).toSeq)(s)

  /**
   * @deprecated: use beTheSameSetAs instead
   */   
  def beSameSetAs[T](s: =>Set[T])(implicit d: Detailed) = beTheSameSetAs(s)(d)
  /**
   * Matches if a set contains the same elements as s, using the equality (in the any order)
   */   
  def beTheSameSetAs[T](s: =>Set[T])(implicit d: Detailed) = (toMatcher(AnyMatchers.be_==(_:T)(d)).toSet)(s)

  /**
   * Matches if the size is n
   */   
  def haveSize(n: Int) = new Matcher[Collection[Any]](){
    def apply(v: => Collection[Any]) = {val collection = v; (collection.size == n, d(collection) + " has size " + n, d(collection) + " doesn't have size " + n)}
  }
}
