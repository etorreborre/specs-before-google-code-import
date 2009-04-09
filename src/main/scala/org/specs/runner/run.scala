import org.specs.util._
import org.specs._
import org.specs.runner._
import org.specs.util.Classes._

object run extends ClassRunner
class ClassRunner extends OutputReporter {
  val timer = new SimpleTimer
  lazy val specs = getSpecifications
  
  override def main(args: Array[String]) = {
    if (args.length == 0)
      println("The first argument should be at least the specification class name")
    else {
      super.main(args)
      reportSpecs
    }
  }
  override protected def displayUsage = {
    println("usage java <classpath> run [className|-k classNames]")
  }
  override protected def displayOptions = {
    super.displayOptions
    println("    [-k|--classes]")
    println("    [-p|--packages]")
  }
  override protected def displayOptionsDescription = {
    super.displayOptionsDescription
    println("-k, --classes                   comma-separated list of specification class names instead")
    println("-p, --packages                  comma-separated list of specification package names to append to class names")
  }

  def getSpecifications: List[Specification] = {
    val packageNames = argValue(args, List("-p", "--packages")).getOrElse("")
    val specificationPackageNames = packageNames.split(",").toList
    val classNames = argValue(args, List("-k", "--classes")).getOrElse(args(0))
    val specificationNames = classNames.split(",").toList
    val specificationClasses = for { 
      packageName <- specificationPackageNames
      className <- specificationNames
    } yield createSpecification(packageName, className)
    specificationClasses.flatMap(x => x)
  }
  def createSpecification(packageName: String, className: String) = {
    createObject[Specification](fullClassName(packageName, className), args.contains("-v"), args.contains("-v"))
  }
  private def fullClassName(packageName: String, className: String) = {
    if (packageName.trim.isEmpty) 
      className.trim
    else 
      packageName.trim+"."+className.trim
  }

}

