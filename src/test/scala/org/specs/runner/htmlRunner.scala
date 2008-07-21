package org.specs.runner

object htmlRunnerSpec extends htmlRunnerRules { "the html runner specification" is <spec>
  
  A specification can be run and its output displayed as an Html page.
  
    1. Specification Title
    
    <ex>The title of the html page should be the title of the specification.</ex>{title}
    
    2. System tables
    
    <ex>There should be a table for each system under test.</ex>{oneTablePerSut}
    <ex>In each table, there should be a row per example</ex>{oneRowPerExample}
    
    3. Example rows
    
    On each row, there should be:
      - <ex>the description of the example</ex>{exampleDescription}
      - <ex>a label "success" if the example succedeed</ex>{exampleSuccess}
      - <ex>a failure message if any</ex>{failedExample}
      - <ex>an exception message if any</ex>{errorExample}
      - <ex>a skip message if any</ex>{skippedExample}
    
</spec>
}
 
import org.specs.specification._
import org.specs.Sugar._
trait htmlRunnerRules extends LiterateSpecification {
  def title = (run must \\(<title>specifiation.name</title>)).shh
  def oneTablePerSut = (run must \\(<th>sut1</th>)).shh
  def oneRowPerExample = (run must \\(<td>ex1</td>)).pp.shh
  def exampleDescription = (run must \\(<td>ex1</td>)).shh
  def exampleSuccess =(run must \\(<td>success</td>)).shh
  def failedExample = (run must \\(<td>'1' is not equal to '0'</td>)).shh
  def errorExample = (run must \\(<td>bug</td>)).shh
  def skippedExample = (run must \\(<td>skipped</td>)).shh
    
  object specification extends Specification("Sample Specification") {
    "sut1" should {
      "ex1" in { 1 must_== 1 }
      "ex2" in { 1 must_== 0 }
      "ex3" in { error("bug") }
      "ex4" in { skip("skipped") }
    }
  }
  def run = { runner.reportSpecs; runner.output }
  object runner extends HtmlRunner(specification)
}
class htmlRunnerTest extends org.specs.runner.JUnit4(htmlRunnerSpec)