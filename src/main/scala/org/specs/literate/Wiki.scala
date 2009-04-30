package org.specs.literate
import org.specs.util.Properties
/**
 * This trait provides functions which can be used to ease the use of wiki markup
 */
trait Wiki extends Properties with Links {
  implicit def toWikiString(a: Any) = new WikiString(a.toString)
  class WikiString(s: String) {
    def code = wikiCode(s)
    def >@ = wikiCode(s)
    def pre = wikiPre(s)
  }
  /**
   * This function can be used to format code in a wiki description.
   * Using this function avoid issues like quotes insides brackets ['something']
   * being displayed as question marks.
   */
  def wikiPre(stringToFormat: String) = <pre>stringToFormat</pre>
  def wikiCode(stringToFormat: String) = stringToFormat.replace("\r\n", "\n").
                                                        replace("\n\r", "\n").
          split("\n").map(htmlize(_)).mkString("==<code class=\"prettyprint\">", "</code>==\n==<code class=\"prettyprint\">", "</code>==")

  protected def htmlize(s: String) = s.replace("<", "&lt;").replace(">", "&gt;")
  /**
   * Alias for wikiCode
   */
  def >@(stringToFormat: String) = wikiCode(stringToFormat)

  def linkTo(susName: String) = "link to " + susName + " not implemented yet"
  override def pathLink(desc: String, path: String) = {
    "\"" + desc + "\":file:///" + path
  }
  override def relativeLink(desc: String, path: String) = {
    "\"" + desc + "\":" + path
  }
  def escapeMarkup = (s: String) => s
}
trait Textile extends Wiki
