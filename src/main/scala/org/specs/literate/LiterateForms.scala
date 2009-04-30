package org.specs.literate
import org.specs.specification._
import org.specs.form._
import org.specs.execute._
/**
 * This trait adds shortcut to declare forms in the specification text
 */
trait LiterateForms extends ExpectableFactory with BaseSpecification { outer =>
  /**
   * This method allows to embbed a Form in a literate specification and display the results of its execution
   */
  implicit def makeForm(s: String) = new LiterateForm(s)
  case class LiterateForm(desc: String) {
    def inForm(form: =>Form) = {
      lazy val formToExecute = form
      val description = if (desc.isEmpty) form.title else desc
      forExample(description) in {
          isExpectation(formToExecute.execute)
          if (!formToExecute.isOk) throw new FailureException("The form '" +  formToExecute.title + "' failed")
      }
      description + "\n" + formToExecute.toHtml.toString
    }
  }
  implicit def toReportableForm(f: Form) = new ReportableForm(f)
  class ReportableForm(f: Form) {
    def report = f.reportTo(outer)
  }
}
