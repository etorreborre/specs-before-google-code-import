/**
 * Copyright (c) 2007-2009 Eric Torreborre <etorreborre@yahoo.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
 * documentation files (the "Software"), to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of
 * the Software. Neither the name of specs nor the names of its contributors may be used to endorse or promote
 * products derived from this software without specific prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED
 * TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS INTHE SOFTWARE.
 */
package org.specs.matcher
import _root_.java.util.concurrent._
import org.specs.specification.{ LifeCycle, Examples, Result }
import Thread.State._
import concurrent.{ Clock, Logging, SpecThreads, ErrorHandler, Conductor }
import _root_.java.util.concurrent.atomic.AtomicReference

trait Threads extends ErrorHandler with Clock with Conductor with LifeCycle with SpecThreads {

  /**
   * Create a new thread that will execute the given function
   * @param f the function to be executed by the thread
   */
  def thread[T](f: => T): Thread = thread("thread" + threads.size) {f}

  /**
   * Create a new thread that will execute the given Runnable
   * @param runnable the Runnable to be executed by the thread
   */
  def thread[T](runnable: Runnable): Thread = thread("thread" + threads.size) {runnable.run}

  /**
   * Create a new thread that will execute the given Runnable
   * @param runnable the Runnable to be executed by the thread
   */
  def thread[T](name: String, runnable: Runnable): Thread = thread(name) {runnable.run}

  /**
   * Create a new thread that will execute the given Callable
   * @param callable the Callable to be executed by the thread
   */
  def thread[T](callable: Callable[T]): Thread = thread("thread" + threads.size) {callable.call}

  /**
   * Create a new thread that will execute the given Callable
   * @param callable the Callable to be executed by the thread
   */
  def thread[T](name: String, callable: Callable[T]): Thread = thread(name) {callable.call}

  /**
   * Create a new thread that will execute the given function
   * @param name the name of the thread
   * @param f the function to be executed by the thread
   */
  def thread[T](name: String)(f: => T): Thread = {
    currentState.get match {
      case TestFinished => throw new IllegalStateException("Test already completed.")
      case _ => {
        val t = new TestThread(name, f _)
        threads add t
        startThread(t)
      }
    }
  }
  
  private val currentState: AtomicReference[ConductorState] = new AtomicReference(Setup)
  // The reason that the thread is started immediately, is do that nested threads
  // will start immediately, without requiring the user to explicitly start() them.

  /**
   * Adds threads methods to int, so one can say:<br/>
   * val threads:List[Thread] = 5.threads("some name"){ ... }<br/>
   * val anonymous_threads:List[Thread] = 10 threads { ... }<br/>
   * @param nrThreads the number of threads to be created
   */
  implicit def addThreadsMethodToInt(nrThreads:Int) = new ThreadedInt(nrThreads)

  class ThreadedInt(nrThreads:Int) {
    def threads[T](name: String)(f: => T): List[Thread] = {
      val seq = for( i <- 1 to nrThreads) yield thread(name + "("+i+")") {f}
      seq.toList
    }
    def threads[T](f: => T): List[Thread] = {
      val seq = for( i <- 1 to nrThreads) yield thread{f}
      seq.toList
    }
  }
  override def beforeExample(ex: Examples) = {
    super.beforeExample(ex)
    testThreadStartLatch.countDown
  }
  override def afterExample(ex: Examples) = {
    conductTest
    super.afterExample(ex)
  }
  /**
   * A test thread runs the given function.
   * It only does so after it is given permission to do so by the main thread.
   * The main thread grants permission after it receives notication that
   * all test threads are ready to go.
   */
  private class TestThread[T](name: String, f: () => T) extends Thread(threadGroup, name) {
    override def run(){
      try {
        // notify the main thread that we are indeed ready to go.
        mainThreadStartLatch.countDown
        // wait for the main thread to say its ok to go.
        testThreadStartLatch.await
        // go
        f()
      } catch {
        // The reason this is a catch Throwable is because you want to let ThreadDeath through
        // without signalling errors. Otherwise the signalError could have been in a finally.
        // If the simulation is aborted, then stop will be called,
        // which will cause ThreadDeath, so just die and do nothing
        case e: ThreadDeath => 
        case t: Throwable => signalError(t)
      }
    }
  }
}
/**
 * Base class for the possible states of the Conductor.
 */
sealed case class ConductorState(testWasStarted:Boolean, testIsFinished: Boolean)

/**
 * The initial state of the Conductor.
 * Any calls the thread{ ... } will result in blocked Threads.
 * Any call to conductTest will start the test.
 */
case object Setup extends ConductorState(false, false)

/**
 * The state of the Conductor while its running.
 * Any calls the thread{ ... } will result in running Threads.
 * Any further call to conductTest will result in an exception.
 */
case object TestStarted extends ConductorState(true, false)

/**
 * The state of the Conductor after all threads have finished,
 * and the whenFinished method has completed.
 * Any calls the thread{ ... } will result in an exception
 * Any call to conductTest will result in an exception.
 */
case object TestFinished extends ConductorState(true, true)
