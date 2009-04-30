package org.specs.literate

/**
 * This trait supports writing literate specifications for acceptance testing.
 * 
 * The basic idea is to define Systems under specification as some text, enclosed in xml tags:
 * <code>
 * "my system" is <text>
 *   some text for my Specification
 * 
 * </text>
 * </code>
 * 
 * Then, it is possible to "tag" parts of this text with the <ex></ex> xml nodes, to denote an example description and make it immediately
 * followed by an expectation in Scala code: <code>
 * "my system" is <text>
 *   <ex>This is an ok example</ex> { 1 must be equalTo(1) }
 * 
 *   <ex>This is an ok example with several expectations which must be enclosed in an 'eg' function</ex> 
 *   { eg { 
 *       1 must be equalTo(1) 
 *       2 must be equalTo(2) 
 *     }
 *   }
 *   <ex>This is a example but missing its implementation yet</ex> { notImplemented }
 * 
 * </text>
 * </code>
 * 
 */
class LiterateSpecification extends Specification with LiterateBaseSpecification with LiterateSpecificationLinks 
      with LiterateDataTables with LiterateForms with LiterateProperties with LiterateShortcuts {
  setSequential()

  def this(n: String) = { this(); name = n; description = n; this }

  /** allow empty sus to be reported. */
  override def filterEmptySus = false
}
