package sudoku

/** Object for validation methods
  */
object Validation {

  /** Converts board of Array[Array[Char]] to Array[Array[Cell]]
    *
    * @param board
    *   Board to convert
    * @return
    *   Fails Left with ValidationErrors or succeeds with Board of Cell Type in Right
    */
  def convertBoard(
      board: Array[Array[Char]]
  ): Either[List[ValidationError], Array[Array[Cell]]] = {
    val results: Array[Array[Either[ValidationError, Cell]]] =
      board.zipWithIndex.map { case (row, rowIndex) =>
        row.zipWithIndex.map { case (char, colIndex) =>
          Cell
            .fromChar(char)
            .toRight(ValidationError.InvalidCell(rowIndex, colIndex, char))
        }
      }

    val errors = results.flatten.collect { case Left(e) => e }.toList

    // getOrElse should never be hit because we ensure all values are Right
    if errors.isEmpty then Right(results.map(_.map(_.getOrElse(Blank))))
    else Left(errors)
  }

  /** Find duplicates in a list of Cells
    *
    * @param cells
    *   List of cells to check
    * @param makeError
    *   Error to make if duplicates detected
    * @return
    *   List of validation errors if any duplicates are found
    */
  private def findDuplicates(
      cells: List[Cell],
      makeError: (Char) => ValidationError
  ): List[ValidationError] = {
    val filled = cells.collect { case Filled(d) => d }
    filled
      .groupBy(identity)
      .collect {
        case (digit, occurrences) if occurrences.length > 1 => makeError(digit)
      }
      .toList
  }

  /** Validates initial board state. Checks:
    *   1. board size is 9 x 9
    *   2. board has no duplicates in rows, cols, and subboxes
    *
    * @param board
    *   Board to check
    * @return
    *   Unit if board is valid else List with all ValidationErrors
    */
  def validateBoard(
      board: Array[Array[Cell]]
  ): Either[List[ValidationError], Unit] = {
    // Check if the board is 9x9
    val dimensionErrors: List[ValidationError] =
      if board.length != Constants.BoardSize || board.exists(
          _.length != Constants.BoardSize
        )
      then List(ValidationError.InvalidBoardSize)
      else Nil

    // Return early - row/col/box checks will crash on wrong sized boards
    if dimensionErrors.nonEmpty then return Left(dimensionErrors)

    val rowErrors: List[ValidationError] =
      board.zipWithIndex.flatMap { case (row, rowIndex) =>
        findDuplicates(
          row.toList,
          char => ValidationError.DuplicateInRow(rowIndex, char)
        )
      }.toList

    val colErrors: List[ValidationError] =
      board.transpose.zipWithIndex.flatMap { case (col, colIndex) =>
        findDuplicates(
          col.toList,
          char => ValidationError.DuplicateInCol(colIndex, char)
        )
      }.toList

    val boxErrors: List[ValidationError] =
      Utils
        .getSubBoxCells(board)
        .zipWithIndex
        .flatMap { case (box, boxIndex) =>
          findDuplicates(
            box,
            char => ValidationError.DuplicateInBox(boxIndex, char)
          )
        }
        .toList

    val allErrors = dimensionErrors ++ rowErrors ++ colErrors ++ boxErrors

    if allErrors.isEmpty then Right(()) else Left(allErrors)
  }

}
