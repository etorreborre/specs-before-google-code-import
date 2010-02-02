/**
 * Copyright 2009 Twitter, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain
 * a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.specs.util

import java.text.{ ParsePosition, SimpleDateFormat }
 
/**
 * This object provides functions to describe units of time
 */
object TimeConversions {
  
  class RichLong(l: Long) {
    def toLong = l
 
    def seconds = new Duration(toLong * 1000)
    def second = seconds
    def milliseconds = new Duration(toLong)
    def millisecond = milliseconds
    def millis = milliseconds
    def minutes = new Duration(toLong * 1000 * 60)
    def minute = minutes
    def hours = new Duration(toLong * 1000 * 60 * 60)
    def hour = hours
    def days = new Duration(toLong * 1000 * 60 * 60 * 24)
    def day = days
  }
 
  implicit def intToRichLong(v: Int) = new RichLong(v.toLong)
  implicit def longToRichLong(v: Long) = new RichLong(v)
}
 
 
/**
 * Use `Time.now` in your app instead of `System.currentTimeMillis`, and
 * unit tests will be able to adjust the current time to verify timeouts
 * and other time-dependent behavior, without calling `sleep`.
 */
object Time {
  import TimeConversions._
 
  private val formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss Z")
 
  private var fn: () => Time = null
  reset()
 
  /**
   * Freeze the clock. Time will not pass until reset.
   */
  def freeze() {
    Time.now = new Time(System.currentTimeMillis)
  }
 
  def now: Time = fn()
  def never: Time = Time(0.seconds)
 
  def now_=(at: Time) {
    fn = () => at
  }
 
  def reset() {
    fn = { () => new Time(System.currentTimeMillis) }
  }
 
  def apply(at: Duration) = new Time(at.inMillis)
 
  def advance(delta: Duration) {
    now = now + delta
  }
 
  def at(datetime: String) = {
    val date = formatter.parse(datetime, new ParsePosition(0))
    if (date == null) {
      throw new Exception("Unable to parse date-time: " + datetime)
    }
    new Time(date.getTime())
  }
}
 
/**
 * Time duration. Along with the conversions provided by the TimeConversions object.
 * Durations can be created by adding the time unit to a number: 1.minute
 */
class Duration(val at: Long) {
  def inDays = (inHours / 24)
  def inHours = (inMinutes / 60)
  def inMinutes = (inSeconds / 60)
  def inSeconds = (at / 1000L).toInt
  def inMillis = at
  def inMilliseconds = at
 
  def +(delta: Duration) = new Duration(at + delta.inMillis)
  def -(delta: Duration) = new Duration(at - delta.inMillis)
 
  def fromNow = Time(Time.now + this)
  def ago = Time(Time.now - this)
 
  override def toString = inSeconds.toString
 
  override def equals(other: Any) = {
    other match {
      case other: Duration => inSeconds == other.inSeconds
      case _ => false
    }
  }
 
  def >(other: Duration) = at > other.at
  def <(other: Duration) = at < other.at
  def >=(other: Duration) = at >= other.at
  def <=(other: Duration) = at <= other.at
}
 
 
class Time(at: Long) extends Duration(at) {
  override def +(delta: Duration) = new Time(at + delta.inMillis)
  override def -(delta: Duration) = new Time(at - delta.inMillis)
}
 

