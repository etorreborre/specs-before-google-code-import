package org.specs.xml

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
  }
}
