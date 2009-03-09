import org.specs.util._
import org.specs._

object run {
  def main(args: Array[String]) = {
    if (args.length == 0)
      println("The first argument should be the specification class name")
    val spec = Classes.createObject[Specification](args(0))
    spec match {
      case Some(s) => s.main(args.drop(1))
      case None => println("The class " + args(0) + " could not be instantiated")
    }
  }
}