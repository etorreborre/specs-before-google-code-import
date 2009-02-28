package org.specs.form
trait Commentable {
    private var commented  = false
    def comment(): this.type = { commented = true; this }
    def uncomment(): this.type = { commented = false; this }
    def isCommented = commented
  }
