package org.specs.matcher.concurrent
import org.specs.execute._

class threadSpec extends org.spex.Specification with org.specs.matcher.Threads {
  val lock = new java.util.concurrent.locks.ReentrantReadWriteLock
  import PimpedReadWriteLock._
  
  "test" in {
    thread("writer thread") {
      waitForTick(2)
      println("got the tick")
        val t = tick
        t must_== 1
      lock.write {
        println("checking the tick: " + t)
        throw new FailureException("failed here with t "+t)
        try {
          t must_== 1
        } catch {
          case e => println("got the expectation exception"); throw e
        }
      }
    }
    thread("reader thread") { 
      println("got the reader tick")
      lock.read { waitForTick(2) }
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
