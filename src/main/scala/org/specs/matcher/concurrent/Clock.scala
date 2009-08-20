package org.specs.matcher.concurrent
import org.specs.matcher.concurrent.PimpedThreadGroup._
  /**
   * A Clock manages the current beat in a MultiThreadedTest.
   * Several duties stem from that responsibility.
   *
   * The clock will:
   *
   * <ol>
   * <li>Block a thread until the tick has reached a particular time.</li>
   * <li>Report the current time</li>
   * <li>Run operations with the clock frozen.</li>
   * </ol>
   */
trait Clock extends Logging with ErrorHandler { outer =>
  /**
   * The metronome used to coordinate between threads.
   * This clock is advanced by the clock thread.
   * The clock will not advance if it is frozen.
   */
  val clock = new ConcurrentClock {
    def printf(m: String, args: Any*) = outer.printf(m, args)
    def println(m: Any) = outer.println(m)
  }
  /**
   * The current beat.
   */
  def tick: Int = clock.tick

  /**
   * When wait for tick is called, the current thread will block until
   * the given tick is reached by the clock.
   */
  // TODO: Could just notify in the advance() method the folks that are waiting on that
  // particular beat, but then that's more complicated. Not a big deal.
  def waitForTick(beat: Int) = clock.waitForTick(beat)
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
case class ClockThread(clockPeriod: Int, maxRunTime: Int) extends Thread("Clock") {
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
}

abstract class ConcurrentClock extends Logging {
  import java.util.concurrent.locks.ReentrantReadWriteLock
  import PimpedReadWriteLock._

  // clock starts at time 0
  private var currentTime = 0
  private val lock = new AnyRef

  /**
   * Read locks are acquired when clock is frozen and must be
   * released before the clock can advance in a waitForBeat().
   */
  private val rwLock = new ReentrantReadWriteLock

  private var highestBeatBeingWaitedOn = 0

  /**
   * Advance the current tick. In order to do so, the clock will wait
   * until it has become unfrozen.
   *
   * All threads waiting for the clock to tick will be notified after the advance.
   *
   * Only the clock thread should be calling this.
   *
   * If the clock has been frozen by a thread, then that thread will own the readLock. Write
   * lock can only be acquired when there are no readers, so ticks won't progress while someone
   * has the clock frozen. Other methods also grab the read lock, like time (which gets
   * the current tick.)
   */
  def advance() {
    lock.synchronized {
      rwLock.write {
        log("clock advancing from: " + currentTime + " to: " + (currentTime + 1))
        currentTime += 1
      } 
      lock.notifyAll()
    }
  }

  /**
   * The current beat.
   */
  def tick: Int = rwLock read currentTime

  /**
   * When wait for tick is called, the current thread will block until
   * the given tick is reached by the clock.
   */
  // TODO: Could just notify in the advance() method the folks that are waiting on that
  // particular beat, but then that's more complicated. Not a big deal.
  def waitForTick(beat: Int) {
    lock.synchronized {
      if (beat > highestBeatBeingWaitedOn) highestBeatBeingWaitedOn = beat
      logAround(currentThread.getName + " is waiting for time " + beat) {
        while (tick < beat) {
          try {
            lock.wait()
          } catch {
            case e: InterruptedException => throw new AssertionError(e)
          }
        }
      }
    }
  }
  // The reason there's no race condition between calling time() in the while and calling
 // lock.wait() later (between that) and some other thread incrementing the tick and doing
  // a notify that this thread would miss (which it would want to know about if that's the
  // new time that it's waiting for) is becauswe both this and the tick method are synchronized
  // on the lock.

  /**
   * Returns true if any thread is waiting for a tick in the future ( greater than the current time )
   */
  def isAnyThreadWaitingForABeat = {
    lock.synchronized {highestBeatBeingWaitedOn > currentTime}
  }

  /**
   * When the clock is frozen, it will not advance even when all threads
   * are blocked. Use this to block the current thread with a time limit,
   * but prevent the clock from advancing due to a    { @link # waitForTick ( int ) } in
   * another thread.
   */
  def withClockFrozen[T](f: => T): T = rwLock read f

  /**
   * Check if the clock has been frozen by any threads.
   */
  def isFrozen: Boolean = rwLock.getReadLockCount > 0
}
