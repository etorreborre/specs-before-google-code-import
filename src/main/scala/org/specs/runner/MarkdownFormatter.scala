package org.specs.runner
import com.petebevin.markdown.MarkdownProcessor

class MarkdownFormatter extends WikiFormatter {
  override protected def parseToHtml(s: String) = {
     val markup = new MarkdownProcessor
     "<div>" + markup.markdown(s) + "</div>"
  }

}
