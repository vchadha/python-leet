object Solution {
    // Constants for the Sudoku board
    val BoardSize: Int = 9
    val BoxSize: Int = 3
    val SubBoxIndecies: Range = (0 until BoxSize * BoxSize)

    // Set of valid digits for Sudoku
    val PossibleDigits: Set[Char] = ('1' to '9').toSet

    // Character representing an empty cell
    val BlankCell: Char = '.'

    // TODO: type wrapper/alias for possible values for a cell
    // Empty as a type
    // NonEmpty as a type that contains the set of possible digits

    /**
      * Solves a given Sudoku puzzle by filling the empty cells.
      * Modifies the input board in-place.
      *
      * @param board A 9x9 2D array representing the Sudoku board, where empty cells are denoted by '.'.
      */
    def solveSudoku(board: Array[Array[Char]]): Unit = {
        
    }

    /**
      * Validates initial board state.
      * Checks:
      *     1. board size is 9 x 9
      *     2. board contains valid characters
      *     3. board has no duplicates in rows, cols, and subboxes
      *
      * @param board Board to check
      * @return True if board is valid else False
      */
    def validateBoard(board: Array[Array[Char]]): Boolean = {
        // Check if the board is 9x9
        (board.length != BoardSize || board.exists(_.length != BoardSize))
        // Check if all characters are valid (digits or blank)
        && board.forall(row => row.forall(cell => PossibleDigits.contains(cell) || cell == BlankCell))
        // Check for duplicates in rows, columns, and boxes
        && board.forall(row => isSetValid(row))
        && board.transpose.forall(col => isSetValid(col))
        && areSubBoxesValid(board)
    }

    /**
      * Determines if a set is valid by checking for no duplicates.
      * Filters out empty cells first.
      *
      * @param set List of values to check
      * @return True if set contains no duplicates after removing empty cells
      */
    def isSetValid(set: Array[Char]): Boolean = {
        // Filter blank cells
        val nonBlankCells = set.filter(_ != BlankCell)

        // No duplicates in row
        nonBlankCells.distinct.length == nonBlankCells.length
    }

    /**
      * Checks if subboxes do not contain duplicates and contain valid digits
      *
      * @param board Sudoku board
      * @return True if all subboxes are valid
      */
    def areSubBoxesValid(board: Array[Array[Char]]): Boolean = {
        SubBoxIndecies.forall(subBoxIndex =>
                val rowStart = (subBoxIndex / BoxSize) * BoxSize
                val colStart = (subBoxIndex % BoxSize) * BoxSize

                // Get all cells for box
                val subBoxCells = for {
                    row <- rowStart until rowStart + BoxSize
                    col <- colStart until colStart + BoxSize
                } yield board(row)(col)

                // Is box set valid
                isSetValid(subBoxCells.toArray)
            )
    }

    /**
      * Get the index of the sub square for a given row and column.
      *
      * | 0 | 1 | 2 |
      * |---|---|---|
      * | 3 | 4 | 5 |
      * |---|---|---|
      * | 6 | 7 | 8 |
      * 
      * @param row Row index
      * @param col Column index
      * @return Index of the sub square (0 to 8)
      */
    def getBoxIndex(row: Int, col: Int): Int =
        (row / BoxSize) * BoxSize + (col / BoxSize)

    def initializeSolutionSets(board: Array[Array[Char]]): (
        Array[Int], // Row sets
        Array[Int], // Column sets
        Array[Int],  // Box sets
        Array[(Int, Int)] // List of empty cell locations
    ) = {
        ???
     // FILTER   // val rowSets = board.map()
    }

}
