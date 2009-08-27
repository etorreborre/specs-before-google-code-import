package org.specs.matcher.concurrent

/*
 * Copyright 2001-2008 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.util.concurrent.atomic.AtomicReference
import _root_.java.util.concurrent.Callable
import org.specs.execute.FailureException
/**
 * A Scala port of <a href="http://code.google.com/p/multithreadedtc/">MultithreadedTC</a>
 * (also <a href="http://www.cs.umd.edu/projects/PL/multithreadedtc/overview.html">here</a>)
 * which was built by Bill Pugh and Nat Ayewah at the University of Maryland.
 *
 * <blockquote>"The MultithreadedTC framework was created to make it easier to test small concurrent abstractions.
 * It enables test designers to guarantee a specific interleaving of two or more threads,
 * even in the presence of blocking and timing issues."</blockquote>
 *
 * <blockquote>"MultithreadedTC is a framework for testing concurrent applications.
 * It features a metronome that is used to provide fine control over the sequence
 * of activities in multiple threads."</blockquote>
 *
 * The Scala version offers significant API improvements over the original Java version,
 * while maintaining most of the intent. Some original ideas were introduced, while some
 * non-scala-like features were cut.  
 *
 * @author Josh Cough
 */
trait ConductorMethods { 

  private val conductor = new AtomicReference[Conductor]()

  /**
   * Create a new thread that will execute the given function.
   * If the test is started, then the thread will run the function immediately.
   * If it is not yet started, the Thread will wait to run the function until
   * all threads are up and ready to go.
   * @param f the function to be executed by the thread
   */
  protected def thread[T](f: => T): Thread = conductor.get.thread { f }

  /**
   * Create a new thread that will execute the given function.
   * If the test is started, then the thread will run the function immediately.
   * If it is not yet started, the Thread will wait to run the function until
   * all threads are up and ready to go.
   * @param name the name of the thread
   * @param f the function to be executed by the thread
   */
  protected def thread[T](name: String)(f: => T): Thread = conductor.get.thread(name){ f }

  /**
   * Create a new thread that will execute the given Runnable
   * @param runnable the Runnable to be executed by the thread
   */
  def thread[T](runnable: Runnable): Thread = conductor.get.thread(runnable)

  /**
   * Create a new thread that will execute the given Runnable
   * @param name the name of the thread
   * @param runnable the Runnable to be executed by the thread
   */
  def thread[T](name: String, runnable: Runnable): Thread = conductor.get.thread(name,runnable)

  /**
   * Create a new thread that will execute the given Callable
   * @param callable the Callable to be executed by the thread
   */
  def thread[T](callable: Callable[T]): Thread = conductor.get.thread(callable)

  /**
   * Create a new thread that will execute the given Callable
   * @param name the name of the thread
   * @param callable the Callable to be executed by the thread
   */
  def thread[T](name: String, callable: Callable[T]): Thread = conductor.get.thread(name,callable)

  /**
   * Force the current thread to block until the thread clock reaches the
   * specified value, at which point the current thread is unblocked.
   * @param c the tick value to wait for
   */
  protected def waitForBeat(beat:Int) = conductor.get.waitForBeat(beat)

  /**
   * Run the passed function, ensuring the clock does not advance while the function is running
   * (has not yet returned or thrown an exception).
   */
  protected def withConductorFrozen[T](f: => T) = conductor.get.withConductorFrozen(f)

  /**
   * Check if the clock has been frozen by any threads. (The only way a thread
   * can freeze the clock is by calling withClockFrozen.)
   */
  protected def isConductorFrozen: Boolean = conductor.get.isConductorFrozen

  /**
   * Gets the current value of the clock. Primarily useful in assert statements.
   * @return the current tick value
   */
  protected def beat = conductor.get.beat

  /**
   * Register a function to be executed after the simulation has finished.
   */
  protected def whenFinished(f: => Unit) = conductor.get.whenFinished{ f }

  /**
   *
   */
  protected def enableLogging() = conductor.get.enableLogging()

  /**
   *
   */
  protected def disableLogging() = conductor.get.disableLogging()

  /**
   * 
   */
  protected var enableLoggingForAllTests = false

  /**
   * Adds threads methods to int, so one can say:<br/> 
   * val threads:List[Thread] = 5.threads("some name")  { ... } <br/>
   * val anonymous_threads:List[Thread] = 10 threads   { ... } <br/>
   * @param nrThreads the number of threads to be created
   */
  protected implicit def addThreadsMethodToInt(nrThreads:Int) = {
    conductor.get.addThreadsMethodToInt(nrThreads)
  }

  /**
   * Secretly sets the conductor to a new Conductor.
   * Then calls super.runTest in order to set up the conductor.
   * Calls the thread, waitForTick, tick, finish all delegate to the current, new Conductor.
   *
   * If the call to super.runTest throws an exception, something obviously went wrong
   * setting up the Conductor, and so the Conductor is not run.
   *
   * If the conductor is set up by super.runTest successfully,  
   */
  private[matcher] def runTest() {

    // use a new conductor for each test
    conductor.compareAndSet(conductor.get,
                           new Conductor(Some(new Informer { def apply(s: String) { println(s) } })))

    if( enableLoggingForAllTests ) conductor.get.enableLogging()
  }

  private[matcher] def runConductor() {

    var caughtException = false

    try {
      conductor.get.conductTest()
    } catch {
      case f: FailureException => 
        throw f
      case e => {
        caughtException = true
        throw e
      }
    } finally {
      // if the main thread threw an exception, then something went
      // really wrong, just get out now.
      // otherwise, handle any errors that occured in test threads
      // if there were errors, fail the test.
      if (!caughtException) {

        val errors = conductor.get.exceptions

        if (!errors.isEmpty) {
          throw new FailureException(errors.head.getMessage)
        }
      }
    }
  }
}