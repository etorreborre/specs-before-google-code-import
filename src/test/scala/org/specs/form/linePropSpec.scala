package org.specs.form

class linePropSpec extends spex.Specification {
  "A LinePropSpec toXhtml method" should {
    "return a <td> cell without the property label" in {
      val p = LineProp("label", 1)
      p.toXhtml must ==/ (<td class="info">1</td>)
    }    
  } 
}
