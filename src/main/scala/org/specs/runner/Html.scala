package org.specs.runner
import scala.xml._
import org.specs.specification._
import org.specs.io._
import org.specs.util._
import org.specs.ExtendedThrowable._

/**
 * The Html trait outputs the results of a specification execution as an html
 * file in a given output directory.
 * 
 * The default file name for the report is "specs-report.html" and that report
 * contains all examples description with their execution status: error, failure, success, skipped.
 */
trait Html extends File {
  val descriptionFormatter = new DescriptionFormatter()
  
  /** definition of the file name of a specification. */
  override def fileName(spec: Specification): String = HtmlNamingFunction.default(spec) 

  /** definition of the output directory of the report. */
  override def outputDir = normalize(".")

  /** report the specification hold by this runner. */
  override def report(specifications: Seq[Specification]) = {
    // reuse the inherited method using the specOutput method
    super.report(specifications)
    // provide the additional resources for the html files
    copySpecResourcesDir("images", outputDir)
    copySpecResourcesDir("css", outputDir)
    this
  }
 
  /** define the html content for this specification execution. */
  def specOutput(spec: Specification): String = asHtml(spec).toString 
  
  /** 
   * Create the html content for this specification execution.
   * 
   * The html page is composed of a left column with a table summarizing the status for all systems
   * and a right (larger) column with a table containing all examples.
   */
  def asHtml(spec: Specification): Elem = <html>
    {head(spec)}
    <body onLoad={onLoadFunction(spec)}>
      <div id="toolTip"/>
      {anchorName("top")}
      {summaryTable(spec)}
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
        <link href="./css/tooltip.css" rel="stylesheet" type="text/css" />
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <script type="text/javascript" src="./css/tooltip.js"/>
        {javaScript}
    </head>
  
  /** return a formatted string depending on the type of literate description: text, wiki or html. */
  def formattedDescription(sus: Sus): Option[Node] = sus.literateDescription map (descriptionFormatter.format(_, sus.examples))

  /** 
   * returns a table with the name of all systems, with their status, 
   * possibly shortened if the system's description is too long. 
   */
  def summaryTable(specification: Specification) = {
    /** returns the title of the specification spanning 2 columns for the summary table. */
    def specNavHeader = {
      <tr>
        <td><img src="images/expanded.gif" onclick="toggleNavBar(this)"/></td>
        <td class="navTitle">{specification.name}</td>
      </tr> 
    }
    <div id="leftColumn">
     { if (nonTrivialSpec(specification)) {
        <table>
          { specNavHeader }
          { summarySpec(specification) }
        </table> }
      else
        NodeSeq.Empty }
    </div>
  }
  
  def summarySpec(spec: Specification): NodeSeq = 
    reduce[Sus](spec.systems, summarySus(_, spec)) ++ 
    reduce[Specification](spec.subSpecifications, summarySpec(_))
    
 /** creates a summary row for a sus. */
  def summarySus(sus: Sus, spec: Specification): NodeSeq = <tr>
	<td>{statusIcon(sus)}</td>
    <td>{anchorRef(susName(sus, spec))}</td>
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
    <h2>{spec.description}</h2> ++ subspecsTables(spec.subSpecifications) ++ susTables(spec)
  }

  /** create tables for systems. */
  def susTables(spec: Specification): NodeSeq = reduce[Sus](spec.systems, susTable(_, spec))
  
  /** create a table for a system. */
  def susTable(sus: Sus, spec: Specification): NodeSeq = {
    anchorName(susName(sus, spec)) ++ 
    susHeader(sus) ++ 
    literateDesc(sus) ++ 
    examplesTable(sus)
  }  

  /** return the sus header if it is not empty, otherwise return the spec name. */
  def susName(sus: Sus, spec: Specification) = if (sus.header.trim.isEmpty) spec.name else sus.header 

  /** return the sus header if not empty or an Empty Node. */
  def susHeader(sus: Sus) = {
    if (!sus.header.trim.isEmpty) 
      <h3>{sus.header}{upArrow}</h3>.toSeq
    else
      NodeSeq.Empty
  }
   
  def examplesTable(sus: Sus) = {
    sus.literateDescription match {
      case None => {
        <table class="bodyTable">
           {exampleRows(sus.examples, sus.isFullSuccess)}
         </table>}
      case Some(_) if !sus.examples.isEmpty => {
        <h3><img src="images/collapsed.gif" onclick={"toggleImage(this); showHideTable('sus:" + System.identityHashCode(sus) + "')"}/>Examples summary</h3>
	    <div id={"sus:" + System.identityHashCode(sus)} style="display:none">
          <table class="bodyTable">
             {exampleRows(sus.examples, sus.isFullSuccess)}
          </table>
        </div>
      }
      case _ => NodeSeq.Empty
    }
  }
  def literateDesc(sus: Sus): NodeSeq = sus.literateDescription match {
    case None => NodeSeq.Empty
    case Some(desc) => formattedDescription(sus).get
  }
  /** create an up arrow with an anchor ref to the top. */
  def upArrow = <a href="#top">   <img src="images/up.gif"/></a>

  /** create rows for each example, alternating style. */
  def exampleRows(examples: Iterable[Example], fullSuccess: Boolean): NodeSeq = examples.toList.foldLeft((NodeSeq.Empty.toSeq, true)) { (result, ex) => 
    val (node, alternation) = result
    (node ++ example(ex, alternation, fullSuccess), !alternation) 
  }._1
  
  /** 
   * create a row for an example and its subexamples.
   * 
   * If the example has subexamples, a small header is created.
   */
  def example(example: Example, alternation: Boolean, fullSuccess: Boolean) = {
    example.subExamples.toList match {
      case Nil => exampleRow(example, alternation, fullSuccess)
      case subexamples => <h4>{formattedDesc(example)}</h4> ++ exampleRows(subexamples, fullSuccess) 
    }  
  }
  def formattedDesc(ex: Example) = {
    descriptionFormatter.formatDesc(ex)
  }
  /**
   * create a row for an example with its status, description and message.
   */
  def exampleRow(example: Example, alternation: Boolean, fullSuccess: Boolean) = {
    <tr class={if (alternation) "b" else "a"}>
      <td id={"rowdesc:" + System.identityHashCode(example)}>{statusIcon(example)} {formattedDesc(example)}</td>{message(example, fullSuccess)}
    </tr>
  }
  
  /**
   * status icon for anything having results (errors, failures, skipped).
   */
  def statusIcon(result: HasResults) = {
    <img src={image(result)} id={"rowicon:" + System.identityHashCode(result)}/>
  }
  def image(result: HasResults) = {
    "images/icon_" + result.status + "_sml.gif"
  }
  
  /** Message for an example. */ 
  def message(example: Example, fullSuccess: Boolean) = {
    def msg = {
	  if (!example.failures.isEmpty)
	      reduce[FailureException](example.failures, failure(_))
	    else if (!example.errors.isEmpty)
	      reduce[Throwable](example.errors, e => exceptionText(e))
	    else if (!example.skipped.isEmpty)
	      reduce[SkippedException](example.skipped, s => exceptionText(s))
	    else
	      ""
    }
    if (fullSuccess)
      NodeSeq.Empty
    else
      <td id={"rowmess:" + System.identityHashCode(example)}>{msg}</td>
  }
  
  /** 
   * the failure message for an example is displayed differently depending on its nature.
   * Failures for DataTables will be reported in a nested table.
   */
  def failure(f: FailureException): NodeSeq = {
    f match {
      case DataTableFailureException(table) => xmlFor(table)
      case regular => exceptionText(regular) 
    }
  }
  def stackTrace(e: Throwable) = if (!e.isInstanceOf[FailureException]) e.stackToString("\r", "\r", "") else ""
  def exceptionText(e: Throwable) = <a title={e.fullLocation + stackTrace(e)}>{if (e.getMessage != null) new Text(e.getMessage) else new Text("null")}</a>
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
      else <td class="noBorder"><img src="images/icon_failure_sml.gif"/></td>
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
  def onLoadFunction(specification: Specification) = {
    if (nonTrivialSpec(specification)) "" else "noNavBar()"
  }
  def nonTrivialSpec(specification: Specification) = {
    (specification.systems ++ specification.subSpecifications).size > 1
  }
  def javaScript = <script language="javascript"> {"""

    // found on : http://www.tek-tips.com/faqs.cfm?fid=6620                                                   
    String.prototype.endsWith = function(str) { return (this.match(str+'$') == str) }

    function changeWidth(id,width) {
      document.getElementById(id).style.width = width;
    }
    function changeMarginLeft(id, margin) {
      document.getElementById(id).style.marginLeft = margin;
    }
    function noNavBar() {
      changeWidth('leftColumn','0px'); 
      changeMarginLeft('bodyColumn', '10px')
   }
   function toggleNavBar(image) {
      toggleImage(image)
      if (image.src.endsWith('images/expanded.gif')) {
		changeWidth('leftColumn','20px'); 
      	changeMarginLeft('bodyColumn', '35px')
	  }
    else {
		changeWidth('leftColumn','250px'); 
      	changeMarginLeft('bodyColumn', '277px')
	  }
   }
   function toggleImage(image) {
	  if (image.src.endsWith('images/expanded.gif')) {
	    image.src = 'images/collapsed.gif';
	  }
    else {
        image.src = 'images/expanded.gif';
	  }
   }
	function showHideTable(tableId) {
	  table = document.getElementById(tableId)
	  table.style.display = (table.style.display == 'none')? 'block' : 'none';
	}
	function showExampleMessage(status, exId, event) {
    exampleMessage = document.getElementById('rowmess:' + exId)
    exampleIcon = document.getElementById('rowicon:' + exId)
    showToolTipWithIcon(status, exampleIcon.src, exampleMessage.innerHTML, event)
  }
	function showExampleDesc(exId, event) {
		exampleDesc = document.getElementById('rowdesc:' + exId)
        showToolTip('Description', exampleDesc.innerHTML, event)
	}
	
"""}
    </script>
}
object HtmlNamingFunction {
  val default = { (s: Specification) => NamingFunction.default(s) + ".html" } 
  val short = { (s: Specification) => NamingFunction.short(s) + ".html" } 
}
