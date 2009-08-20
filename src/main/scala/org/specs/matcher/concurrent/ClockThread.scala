package org.specs.matcher.concurrent
import PimpedThreadGroup._

/**
 * The clock thread is the manager of the MultiThreadedTest.
 * Periodically checks all the test case threads and regulates them.
 * If all the threads are blocked and at least one is waiting for a tick,
 * the clock advances to the next tick and the waiting thread is notified.
 * If none of the threads are waiting for a tick or in timed waiting,
 * a deadlock is detected. The clock thread times out if a thread is in runnable
 * or all are blocked and one is in timed waiting for longer than the runLimit.
 *
 * Algorithm in detail:
 *
 * While there are threads alive
 *
 *    If there are threads RUNNING
 *
 *       If they have been running too long
 *
 *          stop the test with a timeout error
 *
 *    else if there are threads waiting for a clock tick
 *
 *       advance the clock
 *
 *    else if there are threads in TIMED_WAITING
 *
 *       increment the deadlock counter
 *
 *       if the deadlock counter has reached a threadshold
 *
 *          stop the test due to potential deadlock
 *
 *    sleep clockPeriod ms
 *
 *
 * @param mainThread The main test thread. This thread will be waiting
 * for all the test threads to finish. It will be interrupted if the
 * ClockThread detects a deadlock or timeout.
 *
 * @param clockPeriod The period (in ms) between checks for the clock
 *
 * @param maxRunTime The limit to run the test in seconds
 */
abstract case class ClockThread(clockPeriod: Int, maxRunTime: Int) extends Thread("Clock") with SpecThreads with ErrorHandler with Clock { 
  this setDaemon true // TODO: Why is this a daemon thread? If no good reason, drop it.

  // used in detecting timeouts
  private var lastProgress = System.currentTimeMillis

  // used in detecting deadlocks
  private var deadlockCount = 0
  private val MAX_DEADLOCK_DETECTIONS_BEFORE_DEADLOCK = 50

  /**
   * Runs the steps described above.
   */
  override def run {
    while (threadGroup.anyThreadsAlive_?) {
      if (threadGroup.anyThreadsRunning_?) {
        if (runningTooLong_?) timeout()
      }
      else if (clock.isAnyThreadWaitingForABeat) {
        clock.advance()
        deadlockCount = 0
        lastProgress = System.currentTimeMillis
      }
      else if (!threadGroup.anyThreadsInTimedWaiting_?) {
        detectDeadlock()
      }
      Thread sleep clockPeriod
    }
  }

  /**
   * Threads have been running too long (timeout) if
   * The number of seconds since the last progress are more
   * than the allowed maximum run time.
   */
  private def runningTooLong_? = System.currentTimeMillis - lastProgress > 1000L * maxRunTime

  /**
   * Stop the test tue to a timeout.
   */
  private def timeout() {
    val errorMessage = "Timeout! Test ran longer than " + maxRunTime + " seconds."
    signalError(new IllegalStateException(errorMessage))
    mainThread.interrupt()
  }

  /**
   * Determine if there is a deadlock and if so, stop the test.
   */
  private def detectDeadlock() {
    if (deadlockCount == MAX_DEADLOCK_DETECTIONS_BEFORE_DEADLOCK) {
      val errorMessage = "Apparent Deadlock! Threads waiting 50 clock periods (" + (clockPeriod * 50) + "ms)"
      signalError(new IllegalStateException(errorMessage))
      mainThread.interrupt()
    }
    else deadlockCount += 1
  }
}
