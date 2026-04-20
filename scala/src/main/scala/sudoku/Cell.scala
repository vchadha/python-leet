package sudoku

/** Sealed trait to represent cell values
  */
sealed trait Cell
case object Blank              extends Cell
case class Filled(digit: Char) extends Cell

object Cell {

  /** Convert from char to Cell value. Could be Blank or Filled(char)
    *
    * @param c
    *   character to convert
    * @return
    *   Cell representation of char
    */
  def fromChar(c: Char): Option[Cell] = c match
    case Constants.BlankCellChar => Some(Blank)
    case d                       => Filled.fromChar(d)

}

object Filled {

  /** Allowed filled values
    */
  val validFilled: Set[Filled] = Constants.ValidDigits.map(Filled.apply)

  /** Convert from char to Filled Type
    *
    * @param c
    *   character to convert
    * @return
    *   Some(Filled(char)) or None if invalid
    */
  def fromChar(c: Char): Option[Filled] =
    if Constants.ValidDigits.contains(c) then Some(Filled(c))
    else None

}

object CellHelpers {

  /** Filters collection of Cell to just Filled Type
    *
    * @param cells
    *   collection of cells
    * @return
    *   collection of cells with just the filled values
    */
  def toFilledSet(cells: Iterable[Cell]): Set[Filled] =
    cells.collect { case f: Filled => f }.toSet

}
