package typeahead

import org.scalajs.dom.html

import scalatags.JsDom.all._

object FragmentHighlighter {

  def highlightSpans(s: String, frag: String): Seq[html.Element] = {

    val loFrag = frag.toLowerCase

    def spans(s: String, list: List[html.Element]): List[html.Element] = {
      val fragPos = s.toLowerCase.indexOf(loFrag)
      if (fragPos == 0) {
        spans(s.substring(frag.length), list :+ b(s.substring(0,frag.length)).render)
      } else {
        if (fragPos > 0) {
          spans(s.substring(fragPos), list :+ span(s.substring(0, fragPos)).render)
        } else {
          list :+ span(s).render
        }
      }
    }

    spans(s, Nil)
  }
}
