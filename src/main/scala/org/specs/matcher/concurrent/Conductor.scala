package org.specs.matcher.concurrent
import _root_.java.util.concurrent.atomic.AtomicReference
import java.lang.Thread.State._
import org.specs.matcher.concurrent.PimpedThreadGroup._

trait Conductor extends ErrorHandler with Clock {
  /**
   * Run multithreaded test with the default parameters,
   * or the parameters set at the command line.
   */
  def conductTest() {
    val DEFAULT_CLOCKPERIOD = 10
    val DEFAULT_RUNLIMIT = 5
    conductTest(DEFAULT_CLOCKPERIOD, DEFAULT_RUNLIMIT)
  }

  private val currentState: AtomicReference[ConductorState] = new AtomicReference(Setup)

  def testWasStarted = currentState.get.testWasStarted

  /**
   * Start a multithreaded test.
   * @param clockPeriod The period (in ms) between checks for the clock 
   * @param runLimit The limit to run the test in seconds
   * @throws Throwable The first error or exception that is thrown by one of the threads
   */
  def conductTest(clockPeriod: Int, runLimit: Int) {

    // if the test was started already, explode
    // otherwise, change state to TestStarted
    if( testWasStarted ) throw new IllegalStateException("Conductor can only be run once!")
    else currentState set TestStarted

    // wait until all threads are definitely ready to go
    mainThreadStartLatch.await()

    // release the latch, allowing all threads to start
    // wait for all the test threads to start before starting the clock
    testThreadStartLatch.countDown()

    // start the clock thread
    val clockThread = startThread(ClockThread(clockPeriod, runLimit))

    // wait until all threads have ended
    waitForThreads


    // change state to test finished
    currentState set TestFinished
  }

  /**
   * Wait for all of the test case threads to complete, or for one
   * of the threads to throw an exception, or for the clock thread to
   * interrupt this (main) thread of execution. When the clock thread
   * or other threads fail, the error is placed in the shared error array
   * and thrown by this method.
   *
   * @param threads List of all the test case threads and the clock thread
   */
  // Explain how we understand it works: if the thread that's been joined already dies with an exception
  // that will go into errors, and this thread the join will return. If the thread returns and doesn't
  // die, that means all went well, and join will return and it can loop to the next one.
  // There should be no race condition between the last thread being waited on by join, it dies, join
  // returns, and after that the error gets into the errors. Because if you look in run() in the
  // thread inside createTestThread, the signalling error happens in a catch Throwable block before the thread
  // returns.
  private def waitForThreads{
    while(threadGroup.anyThreadsAlive_?){
      threadGroup.getThreads foreach waitForThread
    }
  }

  private def waitForThread(t: Thread) {
    log("waiting for: " + t.getName + " which is in state:" + t.getState)
    try {
      if (t.isAlive && !errorsQueue.isEmpty) logAround("stopping: " + t) {t.stop()}
      else logAround("joining: " + t) {t.join()}
      assert(t.getState == TERMINATED)
    } catch {
      case e: InterruptedException => {
        log("killed waiting for threads. probably deadlock or timeout.")
        errorsQueue offer new AssertionError(e)
      }
    }
  }


}
