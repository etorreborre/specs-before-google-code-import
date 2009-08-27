package org.specs.matcher.concurrent
import org.specs.execute._

class threadSpec extends org.specs.Specification with org.specs.matcher.Threads {
  val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  import PimpedReadWriteLock._

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
