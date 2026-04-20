package sudoku

/** Constants for the board
  */
object Constants {
  val BoardSize: Int = 9
  val BoxSize: Int   = 3

  val SubBoxIndices: Range = 0 until BoxSize * BoxSize

  val ValidDigits: Set[Char] = ('1' to '9').toSet
  val BlankCellChar: Char    = '.'
}
