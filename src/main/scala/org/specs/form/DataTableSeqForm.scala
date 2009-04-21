package org.specs.form

object DataTableSeqForm {
  def apply[T](title: String, seq: Seq[T]) = new DataTableSeqForm(Some(title), seq)
}
class DataTableSeqForm[T](title: Option[String], seq: Seq[T]) extends SeqForm[T](seq) with DataTableFormEnabled {
  override def setHeader[F <: LineForm](line: F): F = super[DataTableFormEnabled].setHeader(line)
}
