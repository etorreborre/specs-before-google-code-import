package org.specs

/**
 * The Function10 to Function20 traits are a complement to the exising Function0 to 9 traits in the standard Scala distribution.<p>
 * The code is generated with the following Ruby script:<pre>
 * N=20
 * def values_decl(n)
 *   (1..n).map{|i| "v#{i-1}: T#{i-1}"}.join(", ")
 * end
 * def Function(i) 
 * "trait Function#{i}[#{(0..i-1).map{|j| "-T#{j}"}.join(", ")}, +R] extends AnyRef {
 *   def apply(#{values_decl(i)}): R
 *   override def toString() = \"<function>\"
 * }"
 * end
 * functions = (10..N).map{|i| Function(i)}.join("\n")
 * puts functions
 * </pre>
 */
trait Function10[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9): R
  override def toString() = "<function>"
}
trait Function11[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10): R
  override def toString() = "<function>"
}
trait Function12[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11): R
  override def toString() = "<function>"
}
trait Function13[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12): R
  override def toString() = "<function>"
}
trait Function14[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13): R
  override def toString() = "<function>"
}
trait Function15[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14): R
  override def toString() = "<function>"
}
trait Function16[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15): R
  override def toString() = "<function>"
}
trait Function17[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16): R
  override def toString() = "<function>"
}
trait Function18[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17): R
  override def toString() = "<function>"
}
trait Function19[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18): R
  override def toString() = "<function>"
}
trait Function20[-T0, -T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] extends AnyRef {
  def apply(v0: T0, v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19): R
  override def toString() = "<function>"
}