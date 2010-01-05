package org.specs.runner

/**
 * Simple implementation of the Notifier trait to display messages in the console
 */
class ConsoleNotifier extends Notifier {
  def runStarting(examplesCount: Int) = println("runStarting - count is " + examplesCount)
  def exampleStarting(name: String) = println("exampleStarting: " + name)
  def exampleSucceeded(name: String) = println("exampleSucceeded: " + name)
  def exampleFailed(name: String, e: Throwable) = println("exampleFailed: " + name + " " + e.getMessage)
  def exampleError(name: String, e: Throwable) = println("exampleError: " + name + " " + e.getMessage)
  def exampleSkipped(name: String) = println("examplekipped: " + name)
  def systemStarting(name: String) = println("systemStarting: " + name)
  def systemSucceeded(name: String) = println("systemSucceeded: " + name)
  def systemFailed(name: String, e: Throwable) = println("systemFailed: " + name + " " + e.getMessage)
  def systemError(name: String, e: Throwable) = println("systemError: " + name + " " + e.getMessage)
  def systemSkipped(name: String) = println("systemSkipped: " + name)
  def systemCompleted(name: String) = println("systemCompleted: " + name)

}
