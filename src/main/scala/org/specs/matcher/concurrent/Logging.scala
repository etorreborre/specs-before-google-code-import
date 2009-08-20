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
package org.specs.matcher.concurrent

import _root_.java.util.concurrent.atomic.AtomicReference

trait Logging extends org.specs.log.Log {
  private val trace: AtomicReference[Boolean] = new AtomicReference(false)

  /**
   * Turn logging on
   */
  def enableLogging() = trace set true

  /**
   * Turn logging off
   */
  def disableLogging() = trace set false

  /**
   * Logs the given object by calling toString on it
   */
  def log(a:Any) = if (trace.get) super.info(a.toString)
  /**
   * Logs before and after executing the given function.
   */
  def logAround[T](a: => Any)(f: => T): T = {
    log("|starting: " + a)
    val t = f
    log("|done with: " + a)
    t
  }

}
