package sudoku

/** Case class to be propagated through recursive calls.
  *
  * @param blankCells
  *   List of locations of blank cells
  * @param candidates
  *   Map of location to possible candidates for that location
  */
case class SolverState(
    blankCells: List[Location],
    candidates: Map[Location, Candidates]
)

object SolverState {

  /** Get solver state from a board. Contains list of locations of blank cells. Contains map of locations to possible
    * candidates for that location.
    *
    * @param board
    *   Board to get SolverState from
    * @return
    *   SolverState
    */
  def from(board: Array[Array[Cell]]): SolverState = {
    // Compute set of numbers in rows, cols, and sub boxes
    // These are only used to get possible values for the initial blank cells
    // Afterwards we simply update the candidates for each blank cell and never
    // need to recompute the row, col, or subbox sets
    val rowSets = board.map(row => CellHelpers.toFilledSet(row)).toVector
    val colSets =
      board.transpose.map(col => CellHelpers.toFilledSet(col)).toVector
    val subBoxSets =
      Utils.getSubBoxCells(board).map(CellHelpers.toFilledSet).toVector

    // Get list of locations for all blank cells
    val blankCells =
      board.zipWithIndex.flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (cell, colIndex) if cell == Blank => Location(rowIndex, colIndex)
        }
      }.toList

    // Create a map of possible values for each blank cell
    val candidates = blankCells.map { location =>
      val possibleValues =
        Filled.validFilled
          -- rowSets(location.row)
          -- colSets(location.col)
          -- subBoxSets(location.subBoxIndex)

      location -> Candidates(possibleValues)
    }.toMap

    SolverState(
      blankCells = blankCells,
      candidates = candidates
    )
  }

}
