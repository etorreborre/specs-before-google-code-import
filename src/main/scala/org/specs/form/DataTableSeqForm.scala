package org.specs.form

class DataTableSeqForm[T](title: Option[String], seq: Seq[T]) extends SeqForm[T](seq) with DataTableFormEnabled {
  def this(title: String, seq: Seq[T]) = this(Some(title), seq)
  override def setHeader[F <: LineForm](line: F): F = super[DataTableFormEnabled].setHeader(line)
}
