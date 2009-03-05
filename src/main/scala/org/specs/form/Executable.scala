package org.specs.form

trait Executable {
  def execute: this.type
}