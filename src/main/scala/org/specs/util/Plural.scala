package org.specs.util

object Plural {
  implicit def stringToPlural(s: String) = StringToPlural(s)
  case class StringToPlural(s: String) {
    def plural(v: Int) = if (v > 1) s+"s" else s    
    def plural(v: Long) = if (v > 1) s+"s" else s
    def bePlural(v: Int) = if (v > 1) s+"are" else s+"is"
    def bePlural(v: Long) = if (v > 1) s+"are" else s+"is"
  }
}
