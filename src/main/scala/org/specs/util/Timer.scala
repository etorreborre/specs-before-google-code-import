package org.specs.util
import java.util.Calendar
import scala.collection.mutable.Stack
/**
 * This trait provides Timer functionalities based on the Java Calendar milliseconds
 */
trait HmsTimer extends Timer {
  /** elapsed time since the last stop */
  var elapsed: Long = 0L

  /** current number of millis when instantiating the object using this Trait */
  var millis: Stack[Long] = new Stack[Long]
  
  /** 
   * starts the with new elapsed time
   */
  def start = {
    elapsed = 0L
    millis.push(Calendar.getInstance.getTime.getTime)
  }

  /** 
   * restarts the Timer with no elapsed time
   */
  def restart = {
    elapsed = 0L
    millis = new Stack[Long]
  }

  /** 
   * Stop the timer, store the number of elapsed millis and return a String representing the time as hour/minute/second/ms
   * @return the elapsed time as a String
   */
  def stop: String = { 
    elapsed = Calendar.getInstance.getTime.getTime - millis.first
    millis.pop
    preciseTime
  }
    
  /** 
   * @return a tuple with the elapsed hours, minutes, seconds and millis 
   */
  def hourMinutesSecondsMillis = {
    var totalMillis = elapsed
    val hours = totalMillis / 1000 / 3600
    totalMillis -= hours * 3600 * 1000
    val minutes = totalMillis / 1000 / 60
    totalMillis -= minutes * 60 * 1000
    val seconds = totalMillis / 1000
    val millis = totalMillis - seconds * 1000
    (hours, minutes, seconds, millis)
  }
  
  /** 
   * @return a formatted string showing the hours, minutes and seconds 
   */
  def hms: String = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    def plural(v: long) = if (v > 1) "s" else ""
    var result = ""
    if (hours > 0) { result += hours + " hour" + plural(hours) + " " } 
    if (minutes > 0) { result += minutes + " minute" + plural(minutes) + " " } 
    result += (seconds + " second" + plural(seconds))
    result
  }
  
  /** 
   * @return a formatted string showing the hours, minutes, seconds and millis 
   */
  def preciseTime: String = {
    val (hours, minutes, seconds, millis) = hourMinutesSecondsMillis
    hms + ", " + millis + " ms"
  }
}

/** 
 * The Timer trait acts as a simple stopwatch. It can be stopped to get the elapsed time as a formatted String.
 */
trait Timer {
  def stop: String
  def hms: String
  def start
  def restart
}

/** 
 * Default class for the HmsTimer trait
 */
class SimpleTimer extends HmsTimer

