package org.specs.runner

class HtmlRunner(val specs: Seq[Specification], outputDirPath: String, fName: Specification => String) extends FileReporter(outputDirPath, fName) with Html {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlRunner() :: XmlRunner())
   * </pre>
   */
  def this() = this(Nil, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with a default value for the output directory. */
  def this(specifications: Specification*) = this(specifications, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with one specification only. */
  def this(specification: Specification, outputDirPath: String) = this(List(specification), outputDirPath, HtmlNamingFunction.default)

  /** Alternative constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: Specification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: Specification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)

}
/**
 * The HtmlSuite class is almost the same as the HtmlRunner class but can be extended with a JUnit trait.
 */
case class HtmlSuite(val specs: Seq[Specification], outputDirPath: String, fName: Specification => String) extends FileSuite(outputDirPath, fName) with Html {
  
  /** 
   * Alternative constructor with no specific specifications and default values.
   * This runner can report specifications using the report method.
   * This capacity is used in conjunction with the Runner class:<pre>
   * object runner extends Runner(spec1 :: spec2, HtmlRunner() :: XmlRunner())
   * </pre>
   */
  def this() = this(Nil, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with a default value for the output directory. */
  def this(specifications: Specification*) = this(specifications, "./", HtmlNamingFunction.default)
  
  /** Alternative constructor with one specification only. */
  def this(specification: Specification, outputDirPath: String) = this(List(specification), outputDirPath, HtmlNamingFunction.default)

  /** Alternative constructor with one specification only. */
  def this(spec: Specification, outputDirPath: String, fName: Specification => String) = this(List(spec), outputDirPath, fName)

  /** definition of the file name of a specification. */
  override def fileName(spec: Specification): String = fName(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(outputDirPath)

}
