package org.specs.runner
import scala.xml._
import org.specs.specification._
import org.specs.io._
import java.io.Writer
import java.net._
class HtmlRunner(specification: Specification, outputDir: String) extends Xml {
  outputDirPath = normalize(outputDir)
  override def fileName = "specs-report.html"

  val specs: Seq[Specification] = List(specification)
  
  def this(spec: Specification) = this(spec, ".")

  override def report(specs: Iterable[Specification]) = {}
  override def reportSpec = {
    super.reportSpec
    copySpecResourcesDir("images", outputDirPath)
    copySpecResourcesDir("css", outputDirPath)
  }
 
  override def specOutput = asHtml(specs(0))
  
  def asHtml(spec: Specification): Elem = <html>
    <head>
      <title>{spec.name}</title>
	    <style type="text/css" media="all">
	      @import url('./css/maven-base.css');
	      @import url('./css/maven-theme.css');
	      @import url('./css/site.css');
	    </style>
        <link rel="stylesheet" href="./css/print.css" type="text/css" media="print" />
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
    </head>
    <body>
    <div id="bodyColumn">
    {subspecsTables(spec.subSpecifications)}
    {sutTables(spec.suts)}
    </div>
    </body>
  </html>
  
  def subspecsTables(subSpecs: List[Specification]): NodeSeq = subSpecs.foldRight(NodeSeq.Empty.toSeq) { (subSpec, node) => 
    node ++ subSpecTable(subSpec) 
  }
  def subSpecTable(subSpec: Specification) = {
    <h2>{subSpec.description}</h2> ++ subspecsTables(subSpec.subSpecifications) ++ sutTables(subSpec.suts)
  }
  def sutTables(suts: List[Sut]): NodeSeq = suts.foldRight(NodeSeq.Empty.toSeq) { (sut, node) => node ++  sutTable(sut) }
  
  def sutTable(sut: Sut): NodeSeq = <h3>{sut.header}</h3>.toSeq ++ <table class="bodyTable">
    {exampleRows(sut.examples)}
    </table>
    
  def exampleRows(examples: Iterable[Example]) = examples.toList.foldLeft((NodeSeq.Empty.toSeq, true)) { (result, ex) => 
    val (node, alternation) = result
    (node ++ exampleRow(ex, alternation), !alternation) 
  }._1
  
  def exampleRow(example: Example, alternation: Boolean) = <tr class={if (alternation) "b" else "a"}>
    <td>{statusIcon(example)}{example.description}</td><td>{message(example)}</td></tr>
    
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
      example.failures.map(_.getMessage).mkString(", ")
    else if (!example.errors.isEmpty)
      example.errors.map(_.getMessage).mkString(", ")
    else if (!example.skipped.isEmpty)
      example.skipped.map(_.getMessage).mkString(", ")
    else
      ""
  }
}