package org.specs.xml
import Xhtml._

class xhtmlSpec extends spex.Specification {
  "The Xhtml object" should {
    "have a spanLastTd function setting a colspan on each last td of a row" in {
      val updated = Xhtml.spanLastTd(
        <table class="dataTable">
          <tr><th>person</th></tr>
          <tr><td>First Name</td><td>Eric</td><td>Last Name</td><td>Torreborre</td></tr>
        </table>)
      updated must (\\(<th>person</th>, Map("colspan"->"4")) and \\(<td>Torreborre</td>, Map("colspan"->"4")))
    }
    "have a spanLastTd function setting a colspan on each last td of a row - 2" in {
      val updated = Xhtml.spanLastTd(
        <table class="dataTable">
          <tr><th>person</th></tr>
          <tr><td>First Name</td><td><b>Eric</b></td></tr>
          <tr><td>First Name</td><td>Eric</td><td>Last Name</td><td>Torreborre</td></tr>
        </table>)
      updated must (\\(<th>person</th>, Map("colspan"->"4")) and \\(<td>Torreborre</td>, Map("colspan"->"4")))
    }
  }
  "The Xhtml object" should {
    "have a maxColSize function computing the maximum size of a row even with <b/> tags nested inside a <td/> tag" in {
      maxColSize(
        <table>
          <tr><td>4/14/2009</td><td>-10</td><td><b>A</b>B</td></tr>
        </table>) must ==(3)
    }

  }
}
