package org.specs.matcher
import org.specs.matcher.MatcherUtils._
import org.specs.util.Plural._
import org.specs.specification.Result
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
    def apply(m: => Iterable[(S, Any)]) = {
      val map = m
      (map.exists{ p => p._1 == k}, 
       dUnquoted(map) + " has the key " + q(k), dUnquoted(map) + " doesn't have the key " + q(k))
    }
  } 
  /**
   * Matches if not(map.contains(k))
   */   
  def notHaveKey[S](k: S) = haveKey(k).not 

  /**
   * Matches if map contains a pair (key, value) with value == v
   */   
  def haveValue[S](v: S) = new Matcher[Iterable[(Any, S)]](){ 
    def apply(m: => Iterable[(Any, S)]) = {
      val map = m
      (map.exists(p => p._2 == v), 
       dUnquoted(map) + " has the value " + q(v), dUnquoted(map) + " doesn't have the value " + q(v))} 
  }

  /**
   * Matches if map doesn't contain a pair (key, value) with value == v
   */   
  def notHaveValue[S](v: S) = haveValue(v).not 

  /**
   * Matches if map contains a pair (key, value) == (k, v)
   */   
  def havePair[S, T](p: (S, T)) = new Matcher[Iterable[(S, T)]](){
     def apply(m: => Iterable[(S, T)]) = {
       val map = m
       (map.exists{case e => e == p}, 
        dUnquoted(map) + " has the pair " + q(p), 
        dUnquoted(map) + " doesn't have the pair " + q(p))}
  }
   

  /**
   * Matches if map doesn't contain a pair (key, value) == (k, v)
   */   
  def notHavePair[S, T](p: (S, T)) = havePair(p).not 
  
  /**
   * Matches if map contains all the specified pairs
   */   
  def havePairs[S, T](pairs: (S, T)*) = new Matcher[Iterable[(S, T)]](){
     def apply(m: => Iterable[(S, T)]) = {
       val map = m
       (pairs.forall(pair => map.exists { case e => e == pair }), 
        dUnquoted(map) + " has the pairs " + q(pairs.mkString(", ")), 
        dUnquoted(map) + " doesn't have the pairs " + q(pairs.mkString(", ")))}
  }
   

  /**
   * Matches if map doesn't contain the specified pairs
   */   
  def notHavePairs[S, T](pairs: (S, T)*) = havePairs(pairs:_*).not 

  /**
   * Matches if the partial function is defined at those values
   */   
  def beDefinedAt[A](values: A*) = new Matcher[PartialFunction[A, Any]](){
    def apply(f: => PartialFunction[A, Any]) = {
      val isDefined = values map {v => (v, f.isDefinedAt(v))}
      val undefined = isDefined filter { !_._2 } map { _._1 }
      (isDefined map {_._2} forall {_ == true}, 
       description.getOrElse("the function") + " is defined for the value".plural(values.size) + " " + q(values.mkString(", ")), 
       description.getOrElse("the function") + " is not defined for the value".plural(undefined.size) + " " + q(undefined.mkString(", ")))
    }
  }
  
  /**
   * Matches if the partial function is defined at those values and return expected values
   */   
  def beDefinedBy[A, B](values: (A, B)*) = new Matcher[PartialFunction[A, B]](){
    def apply(f: => PartialFunction[A, B]) = {
      val isDefined = values map {v => (v, f.isDefinedAt(v._1) && f(v._1) == v._2)}
      val undefined = isDefined filter { !_._2 } map { _._1 }
      (isDefined map {_._2} forall {_ == true}, 
       description.getOrElse("the function") + " is defined by the value".plural(values.size) + " " + q(values.mkString(", ")), 
       description.getOrElse("the function") + " is not defined by the value".plural(undefined.size) + " " + q(undefined.mkString(", ")))
    }
   }
  /** 
   * matcher aliases and implicits to use with BeVerb and HaveVerb 
   */
  implicit def toMapKeyResultMatcher[S, T](result: Result[Map[S, T]]) = new MapKeyResultMatcher(result)
  class MapKeyResultMatcher[S, T](result: Result[Map[S, T]]) {
    def key(k: S) = result.matchWithMatcher(haveKey(k)) 
  }
  implicit def toMapValueResultMatcher[S, T](result: Result[Map[S, T]]) = new MapValueResultMatcher(result)
  class MapValueResultMatcher[S, T](result: Result[Map[S, T]]) {
    def value(k: T) = result.matchWithMatcher(haveValue(k)) 
  }
  implicit def toMapResultMatcher[S, T](result: Result[Map[S, T]]) = new MapResultMatcher(result)
  class MapResultMatcher[S, T](result: Result[Map[S, T]]) {
    def pair(pair: (S, T)) = result.matchWithMatcher(havePair(pair)) 
    def pairs(pairs: (S, T)*) = result.matchWithMatcher(havePairs(pairs:_*)) 
  }
  implicit def toPartialFunctionMatcher[S, T](result: Result[PartialFunction[S, T]]) = new PartialFunctionResultMatcher(result)
  class PartialFunctionResultMatcher[S, T](result: Result[PartialFunction[S, T]]) {
    def definedBy(values: (S, T)*) = result.matchWithMatcher(beDefinedBy(values:_*))
    def definedAt(values: S*) = result.matchWithMatcher(beDefinedAt(values:_*))
  }
  def key[S](k: S) = haveKey(k) 
  def value[S](v: S) = haveValue(v) 
  def pair[S, T](pair: (S, T)) = havePair(pair) 
  def pairs[S, T](pairs: (S, T)*) = havePairs(pairs:_*) 
  def definedBy[S, T](values: (S, T)*) = beDefinedBy(values:_*)
  def definedAt[T](values: T*) = beDefinedAt(values:_*)
}
