package org.specs.form

/**
 * Format values using a default formatter (for Doubles) or a custom one (set with the formatWith  function)
 */
trait ValueFormatter[T] {
  /** value formatter. By default formats Doubles with all decimals */
  private[form] var formatter = (t:Option[T]) => t match {
    case Some(d: Double) => new java.text.DecimalFormat("#.###############").format(d)
    case Some(x: Any) => x.toString
    case None => ""
  }
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def format(s: Option[T]): String = formatter(s)
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def format(s: T): String = format(Some(s))
  /**
   * change the value formatter to display the value differently
   */
  def formatWith(function: Option[T] => String): this.type = { formatter = function; this }
  /**
   * change the value formatter to display the value differently. This formatter displays "" for a missing value
   */
  def formatterIs(function: T => String): this.type = { 
    formatter = (t: Option[T]) => t match {
      case None => ""
      case Some(x) => function(x)
    }
    this 
  }
}
/**
 * Formatter for an Iterable
 */
trait ValuesFormatter[T] {
  /** value formatter. Format decimals properly */
  private var valueFormatter = new ValueFormatter[T] {}.formatter  
  /** values formatter. By default formats lists by inserting commas */
  protected var valuesFormatter = (t:Option[Iterable[T]]) => t match {
    case None => ""
    case Some(x) => x.toList.map((t:T) => formatValue(Some(t))).mkString(", ")
  }
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatValue(s: Option[T]): String = valueFormatter(s)
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatValue(s: T): String = formatValue(Some(s))
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatIterable(s: Option[Iterable[T]]): String = valuesFormatter(s)
  /**
   * format the value. If it is a Double, use a DecimalFormat("#.###############") to display the value
   */
  def formatIterable(s: Iterable[T]): String = valuesFormatter(Some(s))
  /**
   * change the values formatter to display the values differently
   */
  def formatIterableWith(function: Option[Iterable[T]] => String): this.type = { valuesFormatter = function; this }
  /**
   * change the values formatter to display the values differently, using "" for missing values
   */
  def iterableFormatterIs(function: Iterable[T] => String): this.type = { 
    valuesFormatter = (l: Option[Iterable[T]]) => l match {
      case None => ""
      case Some(x) => function(x)
    }
    this 
  }
  /**
   * change the value formatter to display the value differently
   */
  def formatValueWith(function: Option[T] => String): this.type = { valueFormatter = function; this }
  /**
   * change the value formatter to display the value differently. This formatter displays "" for a missing value
   */
  def formatterIs(function: T => String): this.type = { 
    valueFormatter = (t: Option[T]) => t match {
      case None => ""
      case Some(x) => function(x)
    }
    this 
  }
}
