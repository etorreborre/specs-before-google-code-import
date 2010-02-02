package org.specs.util
import org.specs._
import TimeConversions._
import Time._

class timeSpec extends SpecificationWithJUnit {
  
  "A duration" can {
    "be created for 1 minute" in {
      1.minute.inSeconds must_== 60
    }
    "be added to another duration" in {
      (1.minute + 2.seconds).inSeconds must_== 62
    }
  }
  "Time" can {
    "be frozen so different durations can safely refer to 'now'" in {
      Time.freeze
      (1.minute.fromNow - Time.now).inSeconds must_== 60
    }
  }
}
