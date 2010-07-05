package org.specs.collection
import org.specs.util._

/**
 * This trait provides implicit definitions to transform Seqs and Arrays to 
 * lazy parameters. It is used in the IterableMatchers
 */
trait LazyCollections {
  implicit def lazyArray[T](a: =>Array[T]) = new LazyArray(a)
  implicit def lazySeq[T](s: =>Seq[T]) = new LazySeq(s)
  class LazyArray[T](a: => Array[T]) extends LazyParameter[Array[T]](() => a)
  class LazySeq[T](s: => Seq[T]) extends LazyParameter[Seq[T]](() => s)
}
