package org.specs.literate

trait Markdown extends Wiki {
  override def wikiCode(stringToFormat: String) = stringToFormat.replace("\r\n", "\n").
                                                        replace("\n\r", "\n").
          split("\n").map(htmlize(_)).mkString("<code class=\"prettyprint\">", "</code\n<code class=\"prettyprint\">", "</code>")
  override def pathLink(desc: String, path: String) = {
    "[" + desc + "](file:///" + path + ")"
  }
  def pathLink(desc: String, path: String, title: String) = {
    "[" + desc + "](file:///" + path + " " + title + ")"
  }
  override def relativeLink(desc: String, path: String) = {
    "[" + desc + "](" + path + ")"
  }
  def relativeLink(desc: String, path: String, title: String) = {
    "[" + desc + "](" + path + " " + title + ")"
  }
  private val markdownSpecialCharacters = List("\\", "`", "*", "_", "{", "}", "[", "]", "(", ")", "#", "+", "-", ".", "!")
  override def escapeMarkup = (s: String) => markdownSpecialCharacters.foldLeft(s) { (res, cur) => res.replace(cur, "\\" + cur) }
}
