package org.specs.matcher.concurrent

import _root_.java.util.concurrent._
import PimpedThreadGroup._

trait ErrorHandler extends SpecThreads with Logging {
  /**
   * a BlockingQueue containing the first Error/Exception that occured
   * in thread methods or that are thrown by the clock thread
   */
  private[concurrent] val errorsQueue = new ArrayBlockingQueue[Throwable](20)

  /**
   * A list of any errors thrown by test threads at the time this method is called.
   */
  def exceptions: List[Throwable] = {
    def exceptions(errorList: List[Throwable], it: java.util.Iterator[Throwable]): List[Throwable] = {
      if(it.hasNext) exceptions(errorList ::: List(it.next), it)
      else errorList
    }
    exceptions(Nil, errorsQueue.iterator)
  }


  /**
   * Stop all test case threads and clock thread, except the thread from
   * which this method is called. This method is used when a thread is
   * ready to end in failure and it wants to make sure all the other
   * threads have ended before throwing an exception.
   * Clock thread will return normally when no threads are running.
   */
  def signalError(t: Throwable) {
    log(t)
    errorsQueue offer t
    for (t <- threadGroup.getThreads; if (t != currentThread)) {
      log("signaling error to " + t.getName)
      val assertionError = new AssertionError(t.getName + " killed by " + currentThread.getName)
      assertionError setStackTrace t.getStackTrace
      t stop assertionError
    }
  }
}
