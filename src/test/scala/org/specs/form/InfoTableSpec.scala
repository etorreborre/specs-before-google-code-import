package org.specs.form
import org.specs._
import org.specs.Sugar._

class InfoTableSpec extends SpecificationWithJUnit {
  "An info table can be created with a title" in {
    new InfoTable("title").title must_== "title"
  }
  "An info table can be created with some column names for the header" in {
    val table = new InfoTable("title") {
      header("column1", "column2")
    }
    table.toXhtml must \\(<th>column1</th>)
  }
  "An info table can have a line even if the header is missing" in {
    val table = new InfoTable("title") {
      line("value11", "value12")
    }
    table.toXhtml must \\(<td>value11</td>)
  }
  "An info table can have lines with string values" in {
    val table = new InfoTable("title") {
      header("column1", "column2")
      line("value11", "value12")
      line("value21", "value22")
    }
    table.toXhtml must \\(<td>value11</td>) and
                       \\(<td>value22</td>)
  }
  "An info table has lines with values as info values" in {
    val table = new InfoTable("title") {
      line("value11", "value12")
    }
    table.toXhtml must \\(<td class="info">value11</td>)
  }
  "An info table can have lines with values as success values" in {
    val table = new InfoTable("title") {
      line("value11".successValue, "value12")
    }
    table.toXhtml must \\(<td>value11</td>, "class"->"success")
  }
}
