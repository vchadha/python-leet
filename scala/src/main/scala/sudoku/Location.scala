package sudoku

/** Case class representing a location on the board
  *
  * @param row
  *   index of row
  * @param col
  *   index of column
  */
case class Location(row: Int, col: Int):

  /** Function to check if location is a peer of another. Peer is defined as same row or same column
    * or same subbox.
    *
    * @param other
    *   Location to compare against
    * @return
    *   True if location is peer, else False
    */
  def isPeerOf(other: Location): Boolean =
    val sameRow = row == other.row
    val sameCol = col == other.col
    val sameBox = subBoxIndex == other.subBoxIndex

    sameRow || sameCol || sameBox

  /** Get the index of the sub square for a given row and column.
    *
    * | 0   | 1   | 2   |
    * |:----|:----|:----|
    * | 3   | 4   | 5   |
    * | --- | --- | --- |
    * | 6   | 7   | 8   |
    *
    * @param row
    *   Row index
    * @param col
    *   Column index
    * @return
    *   Index of the sub square (0 to 8)
    */
  def subBoxIndex: Int =
    (row / Constants.BoxSize) * Constants.BoxSize + (col / Constants.BoxSize)
