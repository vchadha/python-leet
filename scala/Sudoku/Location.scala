case class Location(row: Int, col: Int)

object Location {
  def isPeer(loc1: Location, loc2: Location) =
    val sameRow = loc1.row == loc2.row
    val sameCol = loc1.col == loc2.col
    val sameBox = Utils.getBoxIndex(loc1) == Utils.getBoxIndex(loc2)
    
    sameRow || sameCol || sameBox
}
