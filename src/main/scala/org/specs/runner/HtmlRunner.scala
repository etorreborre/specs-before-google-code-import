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
  def sutTable(sut: Sut): Elem = <table><th>{sut.description}</th>
    {exampleRows(sut.examples)}
    </table>
    
  def exampleRows(examples: Iterable[Example]) = examples.foldRight(NodeSeq.Empty.toSeq) { (ex, node) => node ++ exampleRow(ex) }
  def exampleRow(example: Example) = <tr><td>{example.description}</td></tr>
}