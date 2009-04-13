package org.specs.runner
import org.specs.io._
import org.specs.util._
import org.specs.log._
import java.io.Writer

/**
 * This trait groups the functionalities of runners creating files from
 * specification executions.
 * 
 * It requires the definition of:<ul>
 * <li>a file name function</li>
 * <li>an output directory function</li>
 * <li>a specs holder having all the specifications to report</li>
 * <li>a specOutput function creating the output string to copy to the target file</li>
 * </ul>
 * 
 * Concrete implementations of that trait can be found in HtmlRunner and XmlRunner.
 */
trait File extends FileSystem with ConsoleLog with SpecsHolder with Console {
  /** @return the file name which should be created */
  def fileName(spec: Specification): String
  
  /** @return the output directory path */
  def outputDir: String
  
  /**
   * the default path is the output dir + file name  
   */
  def filePath(spec: Specification) = normalize(outputDir) + fileName(spec)

  /**
   * get the specification output from specOutput and write it to the target file.
   */
  override def report(specifications: Seq[Specification]): this.type = {
    super.report(specifications)
    mkdirs(outputDir)
    specifications foreach { spec => 
      write(filePath(spec)) { out: Writer =>
        out.write(specOutput(spec))
      }
    }
    this
  }

  /**
   * return the String representing the output of the specification execution.
   */
  def specOutput(spec: Specification): String
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
}

/** 
 * A FileReporter class is used to have a constructor setting the required variables of the File trait.
 */
abstract class FileReporter(outputDirPath: String, fName: Specification => String) extends File {
  override def fileName(s: Specification) = fName(s)
  override def outputDir = normalize(outputDirPath)
}

/** 
 * A FileSuite is like a FileReporter but supports being extended with the JUnit trait
 */
import _root_.org.junit.runner._
@RunWith(classOf[JUnitSuiteRunner])
abstract class FileSuite(outputDirPath: String, fName: Specification => String) extends File {
  override def fileName(s: Specification) = fName(s)
  override def outputDir = normalize(outputDirPath)
}
object NamingFunction {
  import org.specs.util.ExtendedString._
  val default = (s: Specification) => s.getClass.getName.takeWhile(_ != '$').mkString
  val short = (s: Specification) => default(s).split("\\.").toList.last
}

