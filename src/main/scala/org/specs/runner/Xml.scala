package org.specs.runner
import org.specs.io._
import org.specs.util._
import org.specs.log._
import org.specs._
import java.io.Writer
import scala.xml.{Elem, PrettyPrinter}
import org.specs.specification._
import org.specs.ExtendedThrowable._
/**
 * Concrete class for the Xml trait. It allows to select a specification to run and an output path
 * Usage: <code>object runner extends XmlRunner(mySpec, "./results/specs")</code>
 *
 * The name of the generated file is specification.name by default but can be overriden:<pre>
 * object runner extends XmlRunner(mySpec, "./results/specs"){override def fileName="result"}</pre>
 */
class XmlRunner(val specification: Specification, path: String) extends Xml {
  outputDirPath = path
  val specs: Seq[Specification] = List(specification)
  
  /** alternate constructor with the specification only. The output dir is the current directory */
  def this(specification: Specification) = this(specification, ".")
}

/**
 * The <code>XmlRunner</code> trait is used to create an xml file, in a specified output directory
 * with the results of a specification execution.
 * 
 * If the output directory is not specified <pre>object runner extends XmlRunner(mySpec)</pre> then the
 * current directory will be used
 *
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with Xml
 * <code>
 * 
 * The output directory can be overriden if necessary:<pre>
 * class mySpecRunner extends Runner(mySpec) with Xml { override def outputDir = "./results/specs" }</pre>
 */
trait Xml extends FileSystem with ConsoleLog with Console {
  /** private variable storing the output directory path */
  protected var outputDirPath = "."
  
  /** set the output directory path */
  def outputDir_=(path: String) = outputDirPath = path

  /** @return the output directory path */
  def outputDir = outputDirPath
  
  /** @return the first and only spec to execute from the SpecsHolder trait */
  def spec = specs(0)
  
  /**
   * the default name of the file is the specification name + .xml
   */
  def fileName = spec.name + ".xml"
  
  /**
   * the default path is the output dir + specification name  
   */
  def filePath = normalize(outputDirPath) + fileName

  override def reportSpecs = reportSpec
  /**
   * creates the file and write the xml result of the specification execution 
   */
  def reportSpec = {
    report(List(spec))
    mkdirs(outputDirPath)
    write(filePath) { out: Writer =>
      out.write(new PrettyPrinter(200, 2).format(specOutput))
    }
  }

  /**
   * @returns a path with Unix like path separators and a final / separator 
   */
  def normalize(dir: String) = {
    var properDir = dir.replaceAll("\\\\", "/")
    if (!properDir.startsWith("/") && !properDir.startsWith("."))
      properDir = ("./" + properDir)
    if (!properDir.endsWith("/"))
      properDir += "/"
    properDir
  }

  def specOutput = asXml(specs(0))
  /**
   * @returns the specification results translated as to xml (including subspecifications)
   */
  def asXml(s: Specification): Elem = {
    <spec name={s.name} description={s.description} assertions={s.assertionsNb.toString} failures={s.failures.size.toString} errors={s.errors.size.toString}>
      {s.subSpecifications map (asXml(_))}{
       s.suts map (asXml(_))}
    </spec>
  }

  /**
   * @returns the sut results translated as to xml 
   */
  def asXml(sut: Sut): Elem = 
    <sut description={sut.description} assertions={sut.assertionsNb.toString} failures={sut.failures.size.toString} errors={sut.errors.size.toString}>
      {sut.examples map (asXml(_))}
    </sut>

  /**
   * @returns the example results translated as to xml (including sub-examples) 
   */
  def asXml(e: Example): Elem = 
    <example description={e.description} assertions={e.assertionsNb.toString} failures={e.failures.size.toString} errors={e.errors.size.toString}>{
      e.failures map (asXml(_))}{
      e.skipped map (asXml(_))}{
      e.errors map (asXml(_))}{
      e.subExamples map (asXml(_))
    }</example>

  /**
   * @returns an error translated as to xml 
   */
  def asXml(error: Throwable): Elem = <error location={error.location}>{error.getMessage}</error>

  /**
   * @returns a failure translated as to xml 
   */
  def asXml(failure: FailureException): Elem = <failure location={failure.location}>{failure.message}</failure>

  /**
   * @returns a skipped example translated as to xml 
   */
  def asXml(skipped: SkippedException): Elem = <skipped location={skipped.location}>{skipped.message}</skipped>
}
