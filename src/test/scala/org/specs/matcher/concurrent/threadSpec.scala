package org.specs.matcher.concurrent

class threadSpec extends org.spex.Specification with org.specs.matcher.Threads {
  val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  import PimpedReadWriteLock._
  
  "test" in {
    1 must_== 1
    thread("writer thread") {
      waitForTick(1)
      lock.write { tick must_== 1 }
    }
  }
  
  
//  thread("reader thread") { 
//    lock.read { waitForTick(2) }
//  }
//  
//  5.threads("reader thread("+name+")") {
//    lock.read { waitForTick(2) }
//  }
}
