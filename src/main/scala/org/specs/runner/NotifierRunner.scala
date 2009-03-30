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
  def systemCompleted(systemName: String)
}
/**
 * This reporter reports specification by executing them and specifying the results to Notifiers.
 */
class NotifierRunner(val specs: Seq[Specification], val notifiers: List[Notifier]) extends Reporter {
  def this(s: Specification, n: Notifier) = this(List(s), List(n))
  override def report(specs: Seq[Specification]): this.type = {
    super.report(specs)
    val specToRun = if (specs.size == 1)
                      specs(0)
                    else {
                      object totalSpecification extends Specification { include(specs:_*) }
                      totalSpecification
                    }
    
    notifiers.foreach { _.runStarting(specToRun.examplesNb) } 
    reportASpecification(specToRun)
  }
  def reportASpecification(spec: Specification): this.type = {
    notifiers.foreach { _.systemStarting(spec.description) }
    for (subSpec <- spec.subSpecifications) {
      reportASpecification(subSpec)
    }
    for (system <- spec.systems)
      reportSystem(system)
    notifiers.foreach { _.systemCompleted(spec.description) }
    this
  }
  def reportSystem(system: Sus): this.type = {
    notifiers.foreach { _.systemStarting(system.description) }
    for (example <- system.examples)
      reportExample(example)
    notifiers.foreach { _.systemCompleted(system.description) }
    this
  }
  def reportExample(example: Example): this.type = {
    notifiers.foreach { _.exampleStarting(example.description) }
    if (example.isOk)
      notifiers.foreach { _.exampleSucceeded(example.description) }
    else if (!example.failures.isEmpty)
      notifiers.foreach { notifier =>
        example.failures.foreach { failure =>
          notifier.exampleFailed(example.description, failure) 
        }
      }
    else if (!example.errors.isEmpty)
      notifiers.foreach { notifier =>
        example.errors.foreach { error =>
          notifier.exampleError(example.description, error) 
        }
      }
    else if (!example.skipped.isEmpty)
      notifiers.foreach { notifier =>
        notifier.exampleSkipped(example.description) 
      }
    this
  }
}

