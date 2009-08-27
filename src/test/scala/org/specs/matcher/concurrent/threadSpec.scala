package org.specs.matcher.concurrent
import org.specs.execute._

class threadSpec extends org.specs.Specification with org.specs.matcher.Threads {
  val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  import PimpedReadWriteLock._

  "this system" should {
    thread("writer thread") {
      waitForBeat(1)
      "the beat must advance" in { beat must_== 1 }
    }
    thread("reader thread") { 
      lock.read { 
        waitForBeat(2) 
      }
    }
  }
}
