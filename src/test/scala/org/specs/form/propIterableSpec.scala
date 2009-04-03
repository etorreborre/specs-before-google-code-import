package org.specs.form

class propIterableSpec extends spex.Specification {
  "An iterable Prop toString function" should {
    "display iterable values" in {
      PropIterable("label", List(1.234, 2.456)).toString must_== "label: 1.234, 2.456 (expected: _)"
    }
  }
  "An iterable property toXhtml method" should {
    val l = List(1.234, 2.345)
    "display one value only if there is only one" in {
      PropIterable("", List(1.234)).toXhtml must ==/(<td class="value">1.234</td>)
    }
    "display comma-separated values if there are more than one" in {
      PropIterable("", l).toXhtml must ==/(<td class="value">1.234, 2.345</td>)
    }
    "display values with another separator if the valuesFormatter is changed" in {
      val p = PropIterable("", l)
      p.formatIterableWith((l:Option[Iterable[Double]]) => l match { 
        case None => ""
        case Some(x) => x.map(p.formatValue(_)).mkString("/")
      })
      p.toXhtml must ==/(<td class="value">1.234/2.345</td>)
    }
    "display values with another formatter if the valueFormatter is changed" in {
      val p = PropIterable("", l)
      p.formatValueWith((l:Option[Double]) => l match { 
        case None => "0.0"
        case Some(d) => new java.text.DecimalFormat("#.#").format(d)
      })
      p.toXhtml must ==/(<td class="value">1.2, 2.3</td>)
    }
  }


}
