package org.specs.form
import org.specs.xml.Xhtml._
import org.specs.xml.NodeFunctions._
import org.specs.util.ExtendedString._
import org.specs.util.Classes._

/**
 * This trait adds Tabulations to a table
 */
trait Tabs extends Layout { outer => 
  
  /** case class for creating a group of tabs */
  case class tabs() extends Layoutable with LabeledXhtml {
    val label = className(this.getClass).uncamel
    /** adds the group of tabs to the Layout on a new row */
    outer.tr(this)
    var tabValues: List[tab] = Nil
    override def toXhtml = {
      <div class="tabber">{reduce(tabValues.reverse, ((_:ToXhtml).toXhtml))}</div>
    }
    private def addTab(t: tab) = { tabValues = t :: tabValues; this }

    /** case class for creating a tab */
    case class tab(title: String) extends Layoutable {
      /** add the tab to the group of tabs */
      addTab(this)
      /** @return the xhtml in a special div for the tabulation */
      override def toXhtml = {
        <div class="tabbertab" title={title}><table class="dataTable">{spanLastTd(super.xhtml)}</table></div>
      }
    }
  }
}
