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
package org.specs.matcher
import org.specs.util.Duration
import org.specs.util.TimeConversions._

/**
 * This trait provides functions to delay the evaluation of a matcher.
 * 
 * This matcher will be retried a given number of times, using a given sleeping time in between
 */
trait EventuallyMatchers {
  
  /**
   * @return a matcher that will retry the nested matcher a given number of times
   */
  def eventually[T](retries: Int, sleep: Duration)(nested: Matcher[T]): Matcher[T] = new Matcher[T]() {
    def apply(a: => T) = retry(retries, sleep, a)

    def retry(retries: Int, sleep: Duration, a: => T): (Boolean, String, String) = {
      val result = nested(a)
      if (result.success || retries == 1) {
        result
      } else {
        Thread.sleep(sleep.inMillis)
        retry(retries - 1, sleep, a)
      }
    }
  }

  /**
   * @return a matcher that will retry the nested matcher a given 40 times
   */
  def eventually[T](nested: Matcher[T]): Matcher[T] = eventually(40, 100.milliseconds)(nested)
}
object EventuallyMatchers extends EventuallyMatchers 