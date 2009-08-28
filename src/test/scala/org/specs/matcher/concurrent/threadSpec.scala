package org.specs.matcher.concurrent
import org.specs._
import org.specs.matcher._
import java.util.concurrent.locks._
import PimpedReadWriteLock._

class threadSpec extends SpecificationWithJUnit with Threads {
  val lock = new ReentrantReadWriteLock

  "this system" should {
    thread("reader thread") { 
      lock.read { 
        waitForBeat(2) 
      }
    }
    thread("writer thread") {
      waitForBeat(1)
      lock.write { 
        "the beat must advance" in { beat must_== 2 }
      }
    }
  }
}
