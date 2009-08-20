package org.specs.matcher.concurrent

import _root_.java.util.concurrent._

trait SpecThreads extends Logging {
  val threadGroup = new ThreadGroup("Orchestra")

  // all the threads in this test
  val threads = new CopyOnWriteArrayList[Thread]()

  // the main test thread
  val mainThread = currentThread

  /**
   * Keeps the main thread from allowing the test threads to execute their bodies
   * until all of them are started, and ready to go.
   */
  private var mainThreadStartLatch_ : Option[CountDownLatch] = None
  def mainThreadStartLatch = {
    if (mainThreadStartLatch_ == None)
      mainThreadStartLatch_ = Some(new CountDownLatch(threads.size))
    mainThreadStartLatch_.get
   }

  /**
   * Keeps the test threads from executing their bodies until the main thread
   * allows them to.
   */
  val testThreadStartLatch = new CountDownLatch(1)

  /**
   * start a thread, logging before and after
   */
  def startThread(t: Thread): Thread = {
    logAround("starting: " + t) {
      t.start()
      t
    }
  }

}
