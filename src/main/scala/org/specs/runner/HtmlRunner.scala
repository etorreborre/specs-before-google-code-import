package org.specs.runner
import scala.xml._
import org.specs.specification._

class HtmlRunner(spec: Specification) extends ConsoleRunner(spec) {
  def output: NodeSeq = <html>
    <head>
      <title>{spec.name}</title>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>
    <body>
    {sutTables}
    </body>
  </html>
  
  def sutTables: NodeSeq = spec.suts.foldRight(NodeSeq.Empty.toSeq) { (sut, node) => node ++  sutTable(sut) }
  
  def sutTable(sut: Sut): NodeSeq = <h3>{sut.description}</h3>.toSeq ++ <table>
    {exampleRows(sut.examples)}
    </table>
    
  def exampleRows(examples: Iterable[Example]) = examples.toList.foldLeft((NodeSeq.Empty.toSeq, true)) { (result, ex) => 
    val (node, alternation) = result
    (node ++ exampleRow(ex, alternation), !alternation) 
  }._1
  
  def exampleRow(example: Example, alternation: Boolean) = <tr class="{if (alternation) a else b}">
    <td>{statusIcon(example)}</td><td>{example.description}</td>{message(example)}</tr>
    
  def statusIcon(example: Example) = {
    if (!example.failures.isEmpty)
      <img src="images/icon_warning_sml.gif"/>
    else if (!example.errors.isEmpty)
      <img src="images/icon_error_sml.gif"/>
    else if (!example.skipped.isEmpty)
      <img src="images/icon_info_sml.gif"/>
    else
      <img src="images/icon_success_sml.gif"/>
  }
  
  def message(example: Example) = {
    if (!example.failures.isEmpty)
      <td>{example.failures.map(_.getMessage).mkString(", ")}</td>
    else if (!example.errors.isEmpty)
      <td>{example.errors.map(_.getMessage).mkString(", ")}</td>
    else if (!example.skipped.isEmpty)
      <td>{example.skipped.map(_.getMessage).mkString(", ")}</td>
    else
      NodeSeq.Empty.toSeq
  }
}