package sudoku

object TestHelpers {

  // ── Helpers ────────────────────────────────────────────────────────────────
  def board(rows: String*): Array[Array[Char]] =
    rows.map(_.toCharArray.nn).toArray

  def cellBoard(rows: String*): Array[Array[Cell]] =
    rows.map(_.map(Cell.fromChar).map(_.getOrElse(Blank)).toArray).toArray

}
