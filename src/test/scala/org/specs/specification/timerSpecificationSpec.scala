package org.specs.specification
import org.specs._
import org.specs.specification._
import org.specs.runner._
import org.specs.util._
import scala.xml._
import org.specs.Sugar._

class timerSpecificationSpec extends TimerSpecificationActionWords with JUnit {
  "The timer specification" is <p>
   A Simple timer is an object which can measure time. Let's create a timer.
   When a timer is stopped{stop}, the timer should {"fail to return the elapsed time" in failTime} then
   {"return the elapsed time" in succeeds}

   A person can have its name reset. If the person's name is set to {"Peter" as personName},
   then {"the person must be named Peter" in checkName}
</p>
}

class TimerSpecificationActionWords extends LiterateSpecification {
  val simpleTimer = new SimpleTimer
  class Person {var name: String = ""; def setName(n: String) = name = n}
  val person = new Person;
  val Peter = "Peter"
  def stop = simpleTimer.stop.shh
  def failTime = simpleTimer.hms must beMatching("\\d second")
  def succeeds = simpleTimer.hms must beMatching("\\d second")
  def personName = person.setName _
  def checkName = person.name must_== Peter
}

