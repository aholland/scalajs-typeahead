package typeahead

trait SelectionListener[DataType] {
  def itemSelected(item: Option[DataType])
  def selectionComplete()
}