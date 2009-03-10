package org.specs.util
import org.specs._
import org.specs.runner._

class timerSpec extends Specification with JUnit {
  "A timer" should {
    "display 0 seconds if not stopped after being created" in {
      TestTimer().hms must_== "0 second"
    }
    "display the elapsed time if stopped after being started" in {
      val timer = TestTimer()
      timer.start
      timer.currentTime = 1000L
      timer.stop
      timer.hms must_== "1 second"
      timer.preciseTime must beMatching("1 second, \\d+ ms")
    }
    "allow several nested starts and stops returning cumulated times" in {
      val timer = TestTimer()
      timer.start
      timer.start
      timer.currentTime = 1000L
      timer.stop mustMatch "1 second"
      timer.currentTime = 2000L
      timer.stop mustMatch "2 seconds"
    }
  }
  case class TestTimer() extends SimpleTimer {
    var currentTime = 0L
    override def getTime = currentTime
  }
}
