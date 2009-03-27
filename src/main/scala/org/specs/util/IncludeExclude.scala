package org.specs.util

/**
 * This trait adds the possibility to declare some elements as included or excluded.
 * 
 * A filter function is then able to return the list of filtered values for a given set of values
 * according to the values which should be included and the values which should be excluded.
 * 
 * The rule is:
 * 
 * - if there are no included values, the value must not be excluded to be one of the filtered values
 * - if there are included values, the values must both be in the included list and not be in the excluded list to 
 *   be part of the filtered values
 */
trait IncludeExclude[T] {
  private var excluded: List[T] = Nil
  private var included: List[T] = Nil
  def exclude(ex: T*) = excluded = excluded ::: ex.toList
  def include(in: T*) = included = included ::: in.toList
  def reset() = { 
    included = Nil 
    excluded = Nil 
  }
  def filter(values: Seq[T]): Seq[T] = values.filter { v => includeCheck(included, v) && excludeCheck(excluded, v) }
  protected def includeCheck(includedValues: List[T], value: T) = includedValues.isEmpty || includedValues.contains(value) 
  protected def excludeCheck(excludedValues: List[T], value: T) = !excluded.contains(value) 
}
/**
 * Default class for the Include/Exclude behaviour
 */
class DefaultIncludeExclude[T] extends IncludeExclude[T]