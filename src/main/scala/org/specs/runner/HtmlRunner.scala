package org.specs.runner
import scala.xml._
import org.specs.specification._
import org.specs.io._
import org.specs.util._

/**
 * The HtmlRunner class outputs the results of a specification execution as an html
 * file in a given output directory.
 * 
 * The default file name for the report is "specs-report.html" and that report
 * contains all examples description with their execution status: error, failure, success, skipped.
 */
class HtmlRunner(specification: Specification, outputDir: String) extends Xml {
  
  /** Alternative constructor with a default value for the output directory. */
  def this(spec: Specification) = this(spec, ".")

  /** the output directory path is normalized as an unix path ending with a '/'. */
  outputDirPath = normalize(outputDir)

  /** default file name for the generated file. */
  override def fileName = "specs-report.html"

  /** 
   * As a SpecsHolder, the HtmlRunner needs to reference a list of specifications to report.
   * In this case, there is only one.
   */
  val specs: Seq[Specification] = List(specification)
  
  /** report specifications. */
  override def report(specs: Iterable[Specification]) = specs.map(reportSpec(_))

  /** report the specification hold by this runner. */
  override def reportSpec = {
    // reuse the inherited method using the specOutput method
    super.reportSpec
    copySpecResourcesDir("images", outputDirPath)
    copySpecResourcesDir("css", outputDirPath)
  }
 
  /** define the html content for this specification execution. */
  override def specOutput = asHtml(specs(0))
  
  /** 
   * Create the html content for this specification execution.
   * 
   * The html page is composed of a left column with a table summarizing the status for all systems
   * and a right (larger) column with a table containing all examples.
   */
  def asHtml(spec: Specification): Elem = <html>
    {head(spec)}
    <body>
      {anchorName("top")}
  	  <div id="leftColumn">
      {summaryTable}
      </div>        
    <div id="bodyColumn">
      {specificationTable(spec)}
    </div>
    </body>
  </html>

  /** 
   * head declaration for the specification.
   * 
   * The title of the document is the specification name.
   */
  def head(specification: Specification) = <head>
      <title>{specification.name}</title>
	    <style type="text/css" media="all">
	      @import url('./css/maven-base.css');
	      @import url('./css/maven-theme.css');
	      @import url('./css/site.css');
	    </style>
        <link rel="stylesheet" href="./css/print.css" type="text/css" media="print" />
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>


  /** 
   * returns a table with the name of all systems, with their status, 
   * possibly shortened if the system's description is too long. 
   */
  def summaryTable = {
    /** returns the title of the specification spanning 2 columns for the summary table. */
    def specNavHeader = { <tr><td colspan="2" class="navTitle">{spec.name}</td></tr> }
    <table>
      { specNavHeader }
      { reduce[Sut](spec.allSuts, summary(_)) }
    </table>
  }
  
  /** creates a summary row for a sut. */
  def summary(sut: Sut) = <tr>
	<td>{statusIcon(sut)}
    </td><td>{anchorRef(sut.header)}</td>
  </tr>
 
  /** 
   * creates an anchor reference for a given name, 
   * possibly shortening it for the left column display, but leaving the full name
   * as a tooltip.
   */
  def anchorRef(name: String) = {
    <a href={"#" + sanitize(name)} title={name}>{shorten(name)}</a>
  }
  /** creates an anchor name, sanitizing the name. */
  def anchorName(name: String) = <a name={sanitize(name)}/>

  /** sanitize a string so that it can be used as a href */
  def sanitize(s: String) = java.net.URLEncoder.encode(s, "UTF-8")
  
  /** shorten a string to 30 characters maximum. */
  def shorten(s: String) = if (s.size <= 27) s else (s.take(27) + "...")

  /** create tables for all the subspecifications. */
  def subspecsTables(subSpecs: List[Specification]): NodeSeq = reduce[Specification](subSpecs, specificationTable(_))

  /** create a table for one specification. */
  def specificationTable(spec: Specification) = {
    <h2>{spec.description}</h2> ++ subspecsTables(spec.subSpecifications) ++ sutTables(spec.suts)
  }

  /** create tables for systems. */
  def sutTables(suts: List[Sut]): NodeSeq = reduce[Sut](suts, sutTable(_))
  
  /** create a table for a system. */
  def sutTable(sut: Sut): NodeSeq = {
    anchorName(sut.header) ++ 
    <h3>{sut.header}{upArrow}</h3>.toSeq ++ <table class="bodyTable">
    {exampleRows(sut.examples)}
    </table>
  }  

  /** create an up arrow with an anchor ref to the top. */
  def upArrow = <a href="#top">   <img src="images/up.gif"/></a>

  /** create rows for each example, alternating style. */
  def exampleRows(examples: Iterable[Example]): NodeSeq = examples.toList.foldLeft((NodeSeq.Empty.toSeq, true)) { (result, ex) => 
    val (node, alternation) = result
    (node ++ example(ex, alternation), !alternation) 
  }._1
  
  /** 
   * create a row for an example and its subexamples.
   * 
   * If the example has subexamples, a small header is created.
   */
  def example(example: Example, alternation: Boolean) = {
    example.subExamples.toList match {
      case Nil => exampleRow(example, alternation)
      case subexamples => <h4>{example.description}</h4> ++ exampleRows(subexamples) 
    }  
  }
  
  /**
   * create a row for an example with its status, description and message.
   */
  def exampleRow(example: Example, alternation: Boolean) = {
    <tr class={if (alternation) "b" else "a"}>
      <td>{statusIcon(example)}{example.description}</td><td>{message(example)}</td>
    </tr>
  }
  
  /**
   * status icon for anything having results (errors, failures, skipped).
   */
  def statusIcon(result: Specification#HasResults) = {
    def imgName(result: Specification#HasResults) = { 
       if (!result.errors.isEmpty)
         "error"
       else if (!result.failures.isEmpty)
         "warning"
       else if (!result.skipped.isEmpty)
         "info"
       else
         "success"
    }
    <img src="images/icon_{imgName(result)}_sml.gif"/>
  }
  
  /** Message for an example. */ 
  def message(example: Example) = {
    if (!example.failures.isEmpty)
      reduce[FailureException](example.failures, failure(_))
    else if (!example.errors.isEmpty)
      reduce[Throwable](example.errors, e => new Text(e.getMessage))
    else if (!example.skipped.isEmpty)
      reduce[SkippedException](example.skipped, s => new Text(s.getMessage))
    else
      ""
  }
  
  /** 
   * the failure message for an example is displayed differently depending on its nature.
   * Failures for DataTables will be reported in a nested table.
   */
  def failure(f: FailureException): NodeSeq = {
    f match {
      case DataTableFailureException(table) => xmlFor(table)
      case regular => new Text(regular.getMessage) 
    }
  }
  /** Alias for DataTable with all type parameters. */
  type DT = DataTable[T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19] forSome { type T0; type T1; type  T2; type T3; type T4; type T5; type T6; type T7; type T8; type T9; type T10; type T11; type T12; type T13; type T14; type T15; type T16; type T17; type T18; type T19 } 

  /** create an xml nested table for a DataTable. */
  def xmlFor(table: DT) = {
    /** the header contains an empty first cell. */
    val header = fold[Any](<td/>.toSeq)(table.header.titles, s => <td>{s}</td>)

    /** return the <tr/> row for a Row result. */
    def rowResult(result: RowResult) = {
      status(result) ++
      reduce[Any](result.row.valuesList, value => <td>{value.toString}</td>) ++
      failureMessage(result)
    }

    /** 
     * return a message row for a Row result.
     * The message spans all columns.
     */
    def failureMessage(result: RowResult) = {
      result match {
        case RowOk(_) => NodeSeq.Empty
        case RowKo(row, failure) => <tr><td/><td colspan={row.valuesList.size.toString} class="failureMessage">{failure.getMessage}</td></tr>
      }
    }
    /** return a status icon for a Row result. */
    def status(result: RowResult) = {
      if (result.isOk) <td/>
      else <td class="noBorder"><img src="images/icon_warning_sml.gif"/></td>
    }
    /** the rows for DataTable results is the sum of all html rows. */
    val tableRows = reduce[RowResult](table.rowResults, r => <tr>{rowResult(r)}</tr>)

    <table class="nested">{ header ++ tableRows }</table>
  }
  
  /** reduce a list with a function returning a NodeSeq. */
  private def reduce[T](list: Iterable[T], f: T => NodeSeq): NodeSeq = {
    fold[T](NodeSeq.Empty.toSeq)(list, f)
  }
  /** reduce a list with a function and an init NodeSeq value. */
  private def fold[T](initValue: NodeSeq)(list: Iterable[T], f: T => NodeSeq): NodeSeq = {
    list.foldLeft(initValue)( (res, value) => res ++ f(value))
  }
}