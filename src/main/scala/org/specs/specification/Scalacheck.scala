package org.specs
import org.specs.matcher.{ScalacheckMatchers, ScalacheckParameters}

/**
 * This trait can be mixed with a specification to allow the use of Scalacheck in a specification
 */
trait Scalacheck extends ScalacheckMatchers with ScalacheckParameters
