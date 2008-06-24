package org.specs.specification
import org.specs._
import org.specs.specification._
import org.specs.runner.ConsoleRunner
import org.specs.util._
import scala.xml._
import org.specs.Sugar._

object bizRunner extends ConsoleRunner(timerSpecificationSpec)
object timerSpecificationSpec extends TimerSpecificationActionWords {
  "The timer specification" is <p> 
   A Simple timer is an object which can measure time. Let's create a timer.
   When a timer is stopped{stop}, the timer should {"fail to return the elapsed time" in failTime} then
   {"return the elapsed time" in succeeds}
  
   A person can have its name reset. If the person's name is set to {"Peter" as personName}, 
   then {"the person must be named Peter" in checkName}
</p>
}
class TimerSpecification extends LiteralSpecification {
  val timer = new SimpleTimer
  class Person {var name: String = ""; def setName(n: String) = name = n}
  val person = new Person; val Peter = "Peter"
}

class TimerSpecificationActionWords extends LiteralSpecification {
  val timer = new SimpleTimer
  class Person {var name: String = ""; def setName(n: String) = name = n}
  val person = new Person; 
  val Peter = "Peter"
  def stop = timer.stop.shh
  def failTime = timer.hms must beMatching("\\d second")
  def succeeds = timer.hms must beMatching("\\d second")
  def personName = person.setName _
  def checkName = person.name must_== Peter
}

