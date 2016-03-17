package typeahead

import org.scalajs.dom
import org.scalajs.dom.{FocusEvent, MouseEvent, html}

import scala.collection.JavaConversions
import scala.concurrent.Future
import scalatags.JsDom.all._


class TypeAhead[DataType](placeHolder: String, fragPartStringRenderer: DataType => String, dataProvider: String => Future[Seq[DataType]], hintPartStringRenderer: DataType => String = TypeAhead.empty, tabIndex: Int) {

  val inputStyle = "position:absolute; left:0; top:-8; background-color: transparent; outline: none; border-style: none;"

  val searchInput = input(
    style := inputStyle,
    zIndex := "2",
    color := "black",
    `type` := "input",
     tabindex := tabIndex,
    placeholder := placeHolder
  ).render

  val hintInput = input(
    style := inputStyle,
    zIndex := "1",
    color := "grey",
    `type` := "input",
    disabled := "true"
  ).render

  val taSpan = span(
    position := "relative",
    opacity := "0.9",
    searchInput, hintInput).render

  val selectionListeners = new scala.collection.mutable.ListBuffer[SelectionListener[DataType]]()

  def renderOpDivToFragString(item: Option[DiviType]) = if (item.isEmpty) "" else renderDivToFragString(item.get)

  def renderDivToFragString(item: DiviType) = fragPartStringRenderer(item._1)

  def renderOpDivToHintString(item: Option[DiviType]) = if (item.isEmpty) "" else renderDivToHintString(item.get)

  def renderDivToHintString(item: DiviType) = hintPartStringRenderer(item._1)


  import dom.ext._

  import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

  val focusedBorder = "0px 0px 5px 1px #6496E8"

  var searchChars = ""

  searchInput.onfocus = (e: FocusEvent) => {
    searchInput.style.boxShadow = focusedBorder
  }
  searchInput.onblur = (e: FocusEvent) => {
    searchInput.style.boxShadow = "0px 0px 0px 1px lightgrey"
  }

  type DiviType = (DataType, Int, html.Div)
  var divList: Seq[(DataType, Int, html.Div)] = Nil
  var opPrimeHintCandidate: Option[(DataType, Int, html.Div)] = None
  var opSelectedDiv: Option[DiviType] = None

  def update(): Unit = {
    opSelectedDiv = None
    searchChars = searchInput.value
    val frag = searchChars.trim
    if (frag == "") {
      updateList(frag, List.empty)
    } else {
      dataProvider(frag).foreach { itemSequence =>
        updateList(frag, itemSequence)
      }
    }

    def updateList(frag: String, itemSequence: Seq[DataType]) {
      if (frag == searchChars.trim) {
        // <- make sure response isn't out-of-date
        opPrimeHintCandidate = None
        changeSelected(None)
        val loFrag = frag.toLowerCase
        dropdown.innerHTML = ""

        divList = itemSequence.zipWithIndex.map(ci => {
          val item = ci._1
          val renderVal = fragPartStringRenderer(item) + hintPartStringRenderer(item)
          val fragPos1 = renderVal.toLowerCase.indexOf(loFrag)
          (ci._1, ci._2, div(
            FragmentHighlighter.highlightSpans(renderVal, frag),
            position := "float",
            paddingBottom := "5px",
            paddingTop := "5px",
            paddingLeft := "10px",
            paddingRight := "10px"
          ).render)
        })

        divList.foreach((divi: DiviType) => {
          val div = divi._3
          div.onmouseover = (e: MouseEvent) => {
            changeSelected(Some(divi))
          }
          div.onclick = (e: MouseEvent) => {
            changeSelected(Some(divi))
            completeSelection()
          }
        })

        val hintCandidates = divList.filter(renderDivToFragString(_).toLowerCase.startsWith(loFrag))
        opPrimeHintCandidate = if (0 < hintCandidates.length && hintCandidates.length < 3) {
          Some(hintCandidates.head)
        } else {
          None
        }
        divList.foreach(item => dropdown.appendChild(item._3))

        dropdown.style.visibility = if (divList.isEmpty) {
          "hidden"
        }
        else {
          val ddHeight = dropdown.getBoundingClientRect().height
          val wHeight = dom.window.innerHeight
          val sRect = searchInput.getBoundingClientRect()

          val dStyle = dropdown.style
          dStyle.position = "absolute"
          dStyle.top = (if (sRect.bottom + 10 + ddHeight < wHeight) {
            sRect.bottom + 10
          } else {
            sRect.top - 10 - ddHeight
          }) + "px"
          dStyle.left = (searchInput.getBoundingClientRect().left + 5) + "px"
          "visible"
        }

        val pos = (searchInput.getBoundingClientRect().left.toInt + 1) + "px"
        showHintText(frag)
      }
    }
  }

  def showHintText(frag: String): Unit = {
    hintInput.value =
      if (opSelectedDiv.isDefined) {
        frag + (renderOpDivToFragString(opSelectedDiv) + renderOpDivToHintString(opSelectedDiv)).substring(frag.length)
      } else if (opPrimeHintCandidate.isDefined) {
        frag + (renderOpDivToFragString(opPrimeHintCandidate) + renderOpDivToHintString(opPrimeHintCandidate)).substring(frag.length)
      } else {
        ""
      }
  }

  searchInput.oninput = (e: dom.Event) => update()

  searchInput.onkeydown = (e: dom.KeyboardEvent) => {
    var consume: Boolean = false
    val code = e.keyCode
    if (Set(KeyCode.Up, KeyCode.Down, KeyCode.PageUp, KeyCode.PageDown).contains(code)) {
      consume = true
      val newSelection: Option[DiviType] = if (divList.isEmpty) {
        None
      } else Some(
        if (code == KeyCode.PageUp) {
          divList.head
        } else if (code == KeyCode.PageDown) {
          divList.last
        } else {
          val index = ((if (opSelectedDiv.isDefined) opSelectedDiv.get._2 else -1) + (if (code == KeyCode.Up) -1 else 1) + divList.length) % divList.length
          divList(index)
        }
      )
      changeSelected(newSelection)
    }
    if (Set(KeyCode.Right, KeyCode.End, KeyCode.Tab).contains(code)
      && opSelectedDiv.isEmpty
      && opPrimeHintCandidate.isDefined
      && searchInput.selectionStart == searchInput.selectionEnd
      && searchInput.selectionStart == searchInput.value.length) {
      {
        consume = true
        val hint = renderOpDivToFragString(opPrimeHintCandidate)
        if (hint.length > searchChars.length) {
          searchInput.value += hint.substring(searchChars.length, if (code == KeyCode.Right) searchChars.length + 1 else hint.length)
          update()
        }
      }
    }
    if (e.keyCode == KeyCode.Escape) {
      consume = true
      changeSelected(None)
      searchInput.value = searchChars
      showHintText(searchChars)
    }
    if (Set(KeyCode.Enter, KeyCode.Tab).contains(code)) {
      if (opSelectedDiv.isEmpty && opPrimeHintCandidate.isDefined) {
        changeSelected(opPrimeHintCandidate)
      }
      if (opSelectedDiv.isDefined && dropdown.style.visibility == "visible") {
        consume = true
        completeSelection()
      } else if (divList.nonEmpty) {
        consume = true
        changeSelected(Some(divList.head))
      }
    }

    if (consume) {
      e.preventDefault()
    }
  }

  def completeSelection(): Unit = {
    dropdown.innerHTML = ""
    dropdown.style.visibility = "hidden"
    divList = Nil
    //    hintInput.value = ""
    opPrimeHintCandidate = None
    searchInput.value = renderOpDivToFragString(opSelectedDiv)
    showHintText(searchInput.value)
    selectionListeners.foreach(_.selectionComplete())
  }

  def changeSelected(newSelection: Option[DiviType]): Unit = {
    if (newSelection != opSelectedDiv) {
      if (opSelectedDiv.isDefined) {
        opSelectedDiv.get._3.style.backgroundColor = "transparent"
      }
      opSelectedDiv = newSelection
      if (opSelectedDiv.isDefined) {
        val divi = opSelectedDiv.get
        divi._3.style.backgroundColor = "#0099cf"
        searchInput.value = renderDivToFragString(divi)
        showHintText(searchInput.value)
      }
      selectionListeners.foreach(_.itemSelected(if (newSelection.isEmpty) None else Some(newSelection.get._1)))
    }
  }

  lazy val dropdown = div(
    width := "300px",
    border := "solid 1px lightgrey",
    borderRadius := "8px",
    paddingTop := "6px",
    paddingBottom := "6px",
    backgroundColor := "white",
    visibility := "hidden",
    zIndex := "1"
  ).render

}


object TypeAhead {
  val empty: Any => String = _ => ""
}