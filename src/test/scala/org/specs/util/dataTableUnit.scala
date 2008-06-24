package org.specs.util
import org.specs._
import org.specs.specification._
import org.specs.runner._

class datatableTest extends JUnit3(dataTableUnit)
object dataTableUnit extends Specification with DataTables {
  "a data table" should {
    "be just a datarow if it has one single row" in {
      val data = "a"|"b"|"c"|
                  1 ! 2 ! 3 |
   
      data.getClass.getName must beMatching("DataRow")
    }
    "be a datatable if it has at least 2 rows" in {
      val data = "a"|"b"|"c"|
                  1 ! 2 ! 3 | 
                  1 ! 2 ! 3 |> {(a: Int, b: Int, c: Int) => ()}
   
      data.getClass.getName must beMatching("DataTable")
    }
    "have a toString method printing out all its rows, in the user-defined order, separated by a new line" in {
      val datatable = "a"|"b"|"c"|
                       1 ! 0 ! 1 | 
                       1 ! 1 ! 2 | 
                       1 ! 2 ! 3 |> {(a: Int, b: Int, c: Int) => ()}
   
      datatable.toString must_== "|a|b|c|\n" +
                                 "|1|0|1|\n" +
                                 "|1|1|2|\n" + 
                                 "|1|2|3|"
    }
    "provide the results of the execution of a function over all rows" in {
      val datatable = "a"|"b"|"c"|
                       1 ! 2 ! 3 | 
                       1 ! 2 ! 3 |> {(a: Int, b: Int, c: Int) => true} 
   
      datatable.results must_== "|a|b|c|\n|1|2|3|\n|1|2|3|"
    }
    "fail if at least on row fails the function" in {
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 3 | 
                         1 ! 2 ! 3 |{(a: Int, b: Int, c: Int) => throw FailureException("fail")}
       try { datatable.execute } catch { case _ => true }
      
      datatable.failed mustBe true
    }
    "provide the results of the execution of a function over all rows, showing failures if the function throws an exception" in {
       val datatable =  "a"|"b"|"c"|
                         1 ! 2 ! 4 | 
                         1 ! 2 ! 3 |{(a: Int, b: Int, c: Int) => a+b must_== c}
       try { datatable.execute }
       catch { case _ => true }
      
      datatable.results must_== " |a|b|c|\n" + 
                                "x|1|2|4| '3' is not equal to '4'\n" +
                                " |1|2|3|"
    }
    "allow type inference on cell types" in {
      val row1 = true ! true
      val row2 = 1    ! true 
      val t = row1 | row2; ()
      
    }
  }
}
