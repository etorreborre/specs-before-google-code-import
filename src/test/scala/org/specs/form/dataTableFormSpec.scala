package org.specs.form

class dataTableFormSpec extends spex.Specification {
  "A data table form" can {
    "be executed as a Form" in {
      
      val f = new DataTableForm {
        "a" | "b" | "sum" |
         1  !  1  !   2   |
         2  !  3  !   5   |> { (a:Int, b:Int, sum:Int) =>
           tr(field(a), field(b), prop(a + b)(sum))
         }
      }
      (f.toXhtml \("tr"))(1) must ==/(<tr><th>a</th><th>b</th><th colspan="3">sum</th></tr>)
    }
  }
}
