package org.specs.runner

import org.specs.io.FileSystem
import java.util.regex._
import scala.collection.mutable.Queue
import org.specs.util.Classes._

/**
 * Companion SpecsFinder object to create a SpecsFinder returning an aggregate Specification of 
 * all the found specifications.
 */
object SpecsFinder {
  def apply(path: String, pattern: String) = new SpecsFinder(path, pattern, true)
}

/**
 * The SpecsFinder class can be used to hold the specifications found on a given path, with a given pattern.
 * Those specifications will either be combined into a big one or kept separate as a Sequence.
 */
case class SpecsFinder(path: String, pattern: String, asOneSpecification: Boolean) extends SpecificationsFinder with SpecsHolder {

  lazy val specs = collectSpecs(asOneSpecification)

  private def collectSpecs(asOneSpecification: Boolean) = {
    val collected = new scala.collection.mutable.ListBuffer[Specification]
    val specNames = specificationNames(path, pattern)
    specNames foreach {className => 
      createSpecification(className) match {
        case Some(s) => collected.append(s)
        case None => scala.Console.println("Could not load " + className)
      }
    }
    if (asOneSpecification) {
	    object totalSpecification extends Specification {
	      new java.io.File(path).getAbsolutePath isSpecifiedBy(collected: _*)
	    }
	    List(totalSpecification)
    } 
    else
      collected.toList 
  }  
}
/**
 * This trait browses files on a given path and returns what can be matching specification class names. 
 */  
trait SpecificationsFinder extends FileSystem {

   /** 
    * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
    * @param pattern a regular expression which is supposed to match an object name extending a Specification
    * @return specification names by scanning files and trying to find specifications declarations
    */
   def specificationNames(path: String, pattern: String) : List[String] = {
     var result = new Queue[String]
     filePaths(path).foreach { collectSpecifications(result, _, pattern) }
     result.toList
   }
  
  /**
   * adds possible specification class names found in the file <code>filePath</code><br>
   * The specification pattern is: "\\s*object\\s*(" + pattern + ")\\s*extends\\s*.*Spec.*\\s*\\{"
   * This may be later extended to support other arbitrary patterns 
   *
   * @param path a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
   * @param pattern a regular expression which is supposed to match an object name extending a Specification
   */
  def collectSpecifications(result: Queue[String], filePath: String, pattern: String): Unit = {
    if (!filePath.endsWith(".scala")) return    
    val specPattern = "\\s*object\\s*(" + pattern + ")\\s*extends\\s*.*Spec.*\\s*\\{"
    val m = Pattern.compile(specPattern).matcher(readFile(filePath))
    while (m.find) {
      result += ((packageName(filePath).map(_ + ".").getOrElse("") + m.group(1).trim) + "$")
    }
  }
  
  /** @return the package declaration at the beginning of a file */
  def packageName(path: String) = {
    val pattern = "\\s*package\\s*(.+)\\s*"
    val m = Pattern.compile(pattern).matcher(readFile(path))
    if (!m.find)
      None 
    else 
      Some(m.group(1).replace(";", "").trim)
  }
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   * @return None in case of an exception. 
   */
  def createSpecification(className: String): Option[Specification] = createObject[Specification](className, false)
}
