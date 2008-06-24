package org.specs.runner
import org.specs.log.ConsoleLog
import org.specs.collection.JavaCollectionsConversion
import _root_.org.junit.runner._

/**
 * The SpecsHolder trait can be inherited by runners to get access to the specifications to report 
 */  
trait SpecsHolder {
  val specs: Seq[Specification]
}

/**
 * The Runner class is an abstract class referencing one or several specifications to run. It should be extended
 * with at least one runner trait which will use the <code>specifications</code>.
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with JUnit with Console with ScalaTest
 * or
 * class mySpecRunner extends Runner(mySpec) with JUnit with Xml with ScalaTest
 * which will also output the results in an xml file
 * <code>
 * 
 * Then mySpecRunner class can be executed in many ways:<ul>
 * <li>scala -cp ... -e "new mySpecRunner.reportSpecs"
 * <li>java -cp ... org.scalatest.Runner -g -s mySpecRunner
 * <li>java -cp ... org.junit.runner.JUnitCore mySpecRunner
 * </ul>
 * 
 * It is annotated with a JUnit annotation because JUnit requires that annotation should be placed on the class which will be executed. 
 * In the example above, Runner(mySpec) is the only class; JUnit, Console and ScalaTest are all traits.
 */  
@RunWith(classOf[JUnitSuiteRunner])
abstract class Runner(var specifications: Specification*) extends SpecsHolder {
  /** alternate constructor with a list of specifications */  
  val specs: Seq[Specification] = specifications

  /** alternate constructor with a list of specifications */  
  def this(specifications: List[Specification]) = this(specifications:_*)
}

/**
 * This trait can be mixed in with a Runner class to output the result of a specification to the console 
 * Usage:<code>
 * class mySpecRunner extends Runner(mySpec) with Console
 * <code> 
 */  
trait Console extends ConsoleReporter with SpecsHolder {
  /**
   * optional arguments to the main method if called from the code directly
   */
  var args: Array[String] = Array()
  def reportSpecs = {
    if (args.exists(List("-ns", "--nostacktrace").contains(_))) setNoStacktrace
    report(specs) 
  }
  def main(arguments: Array[java.lang.String]) = {
    args = args ++ arguments
    reportSpecs
    if (specs.exists { _.isFailing }) System.exit(1) else System.exit(0)
  }
}

/**
 * This class implements the <code>Console</code> trait and can be initialized with specifications directly<br>
 * Usage: <code>object mySpecRunner extends ConsoleRunner(mySpec1, mySpec2)</code>
 */  
class ConsoleRunner(val specifications: Specification*) extends Console {
  val specs = specifications 
  def ConsoleRunner(specs: List[Specification]) = new ConsoleRunner(specs :_*)
}

/**
 * This class can be used to search for specifications on a given path 
 * and execute them.<br>
 * Usage: <code>object myFileRunner extends SpecsFileRunner(path, pattern)</code><br>
 * Where <code>path</code> is a path to a directory containing scala files (it can be a glob: i.e. "dir/**/*spec.scala")
 * and <code>pattern</code> is a regular expression which is supposed to match an object name extending a Specification
 * class named ".*Spec.*"
 */  
class SpecsFileRunner(path: String, pattern: String) extends Console with SpecsFinder {
  val specs = new scala.collection.mutable.ListBuffer[Specification]
  
  /** 
   * overrides the <code>reportSpecs</code> method in the <code>Console</code> trait.<p>
   * The list of specifications to report is build from specification names found on the path
   * indicated by the <code>path</code> parameter
   */
  override def reportSpecs = {
    specificationNames(path, pattern) foreach {className => 
      createSpecification(className) match {
        case Some(s) => specs.prepend(s)
        case None => println("Could not load " + className)
      }
    }
    // this specification is added for better reporting
    object totalSpecification extends Specification {
      new java.io.File(path).getAbsolutePath isSpecifiedBy(specs: _*)
    }
    super.report(List(totalSpecification))
  } 
  /**
   * @return a <code>Specification</code> object from a className if that class is a <code>Specification</code> class.<br>
   * Tries to load the class name and cast it to a specification
   * @return None in case of an exception. 
   */
  def createSpecification(className: String): Option[Specification] = {
    try {
     return Some(getClass.getClassLoader.loadClass(className).newInstance.asInstanceOf[Specification])
    } catch {
      case e => ()
    }
    return None
  }
}