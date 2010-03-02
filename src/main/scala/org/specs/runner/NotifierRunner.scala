/**
 * Copyright (c) 2007-2010 Eric Torreborre <etorreborre@yahoo.com>
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
 * DEALINGS IN THE SOFTWARE.
 */
package org.specs.runner
import org.specs.specification._

/**
 * This is a generic trait for defining a notified object which will know about the state of a run.
 */
trait Notifier {
  def runStarting(examplesCount: Int)
  def exampleStarting(exampleName: String)
  def exampleSucceeded(testName: String)
  def exampleFailed(testName: String, e: Throwable)
  def exampleError(testName: String, e: Throwable)
  def exampleSkipped(testName: String)
  def systemStarting(systemName: String)
  def systemSucceeded(testName: String)
  def systemFailed(testName: String, e: Throwable)
  def systemError(testName: String, e: Throwable)
  def systemSkipped(testName: String)
  def systemCompleted(systemName: String)
}
/**
 * This reporter reports specification by executing them and specifying the results to Notifiers.
 */
class NotifierRunner(val specs: Array[Specification], val notifiers: Array[Notifier]) extends Reporter {
  def this(s: Specification, n: Notifier) = this(Array(s), Array(n))
  override def report(specs: Seq[Specification]): this.type = {
    super.report(specs)
    val specToRun = if (specs.size == 1)
                      specs(0)
                    else {
                      object totalSpecification extends Specification { include(specs:_*) }
                      totalSpecification
                    }
    
    notifiers.foreach { _.runStarting(specToRun.firstLevelExamplesNb) } 
    reportASpecification(specToRun, specToRun.planOnly())
  }
  def reportASpecification(spec: Specification, planOnly: Boolean): this.type = {
    notifiers.foreach { _.systemStarting(spec.description) }
    for (subSpec <- spec.subSpecifications) {
      reportASpecification(subSpec, planOnly)
    }
    for (system <- spec.systems)
      reportSystem(system, planOnly)
    notifiers.foreach { _.systemCompleted(spec.description) }
    this
  }
  def reportSystem(system: Sus, planOnly: Boolean): this.type = {
    notifiers.foreach { _.systemStarting(system.header) }
    
    if (!planOnly && !system.ownFailures.isEmpty)
      notifiers.foreach { notifier =>
        system.ownFailures.foreach { failure =>
          notifier.systemFailed(system.description, failure) 
        }
      }
    else if (!planOnly && !system.ownErrors.isEmpty)
      notifiers.foreach { notifier =>
        system.ownErrors.foreach { error =>
          notifier.systemError(system.description, error) 
        }
      }
    else if (!planOnly && !system.ownSkipped.isEmpty)
      notifiers.foreach { notifier =>
        system.ownSkipped.foreach { skipped =>
          notifier.systemSkipped(skipped.getMessage)
        }
      }
    for (example <- system.examples)
      reportExample(example, planOnly)
    notifiers.foreach { _.systemCompleted(system.header) }
    this
  }
  def reportExample(example: Examples, planOnly: Boolean): this.type = {
    notifiers.foreach { _.exampleStarting(example.description) }
    
    if (!planOnly)
      example.examples.foreach(e => reportExample(e, planOnly))
    if (!planOnly && example.isOk)
      notifiers.foreach { _.exampleSucceeded(example.description) }
    else if (!planOnly && !example.failures.isEmpty)
      notifiers.foreach { notifier =>
        example.failures.foreach { failure =>
          notifier.exampleFailed(example.description, failure) 
        }
      }
    else if (!planOnly && !example.errors.isEmpty)
      notifiers.foreach { notifier =>
        example.errors.foreach { error =>
          notifier.exampleError(example.description, error) 
        }
      }
    else if (!planOnly && !example.skipped.isEmpty)
      notifiers.foreach { notifier =>
        notifier.exampleSkipped(example.description) 
      }
    this
  }
}

