package org.specs.matcher
import org.specs.matcher.MatcherUtils._

/**
 * The <code>MapMatchers</code> trait provides matchers which are applicable to Map objects<br>
 * It currently accepts any Iterable[(K, V)] whereas it should only accept Map[K, V]. 
 * This is because the implicit defs in the <code>SpecificationStructure</code> trait
 * are picking up iterables in general
 */
trait MapMatchers {

  /**
   * Matches if map.contains(k)
   */   
  def haveKey[S](k: S) = new Matcher[Iterable[(S, Any)]](){ 
    def apply(m: => Iterable[(S, Any)]) = {val map = m; (map.exists{ p => p._1 == k}, map + " has key " + q(k), map + " hasn't key " + q(k))}
  } 

  /**
   * Matches if not(map.contains(k))
   */   
  def notHaveKey[S](k: S) = haveKey(k).not 

  /**
   * Matches if map contains a pair (key, value) with value == v
   */   
  def haveValue[S](v: S) = new Matcher[Iterable[(Any, S)]](){ 
    def apply(m: => Iterable[(Any, S)]) = {val map = m; (map.exists(p => p._2 == v), map + " has value " + q(v), map.toString + " hasn't value " + q(v))} 
  }

  /**
   * Matches if map doesn't contain a pair (key, value) with value == v
   */   
  def notHaveValue[S](v: S) = haveValue(v).not 

  /**
   * Matches if map contains a pair (key, value) == (k, v)
   */   
  def havePair[S, T](p: (S, T)) = new Matcher[Iterable[(S, T)]](){
     def apply(m: => Iterable[(S, T)]) = {val map = m; (map.exists{case e => e == p}, map + " has pair " + q(p), map + " hasn't pair " + q(p))}
  }
   

  /**
   * Matches if map doesn't contain a pair (key, value) == (k, v)
   */   
  def notHavePair[S, T](p: (S, T)) = havePair(p).not 
  
  /**
   * Matches if the partial function is defined at those values
   */   
  def beDefinedAt[A](values: A*) = new Matcher[PartialFunction[A, Any]](){
    def apply(f: => PartialFunction[A, Any]) = {
      val isDefined = values map {v => (v, f.isDefinedAt(v))}
      val undefined = isDefined filter { !_._2 } map { _._1 }
      (isDefined map {_._2} forall {_ == true}, "the function is defined for " + plural("values", values.size) + " " + q(values.mkString(", ")), "the function is not defined for " + plural("value", undefined.size) + " " + q(undefined.mkString(", ")))
    }
  }
  
  /**
   * Matches if the partial function is defined at those values and return expected values
   */   
  def beDefinedBy[A, B](values: (A, B)*) = new Matcher[PartialFunction[A, B]](){
    def apply(f: => PartialFunction[A, B]) = {
      val isDefined = values map {v => (v, f.isDefinedAt(v._1) && f(v._1) == v._2)}
      val undefined = isDefined filter { !_._2 } map { _._1 }
      (isDefined map {_._2} forall {_ == true}, "the function is defined by " + plural("values", values.size) + " " + q(values.mkString(", ")), "the function is not defined by " + plural("value", undefined.size) + " " + q(undefined.mkString(", ")))
    }
   }
}
