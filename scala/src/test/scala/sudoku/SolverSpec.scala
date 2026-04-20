package sudoku

import sudoku.TestHelpers.{board, cellBoard}

class SolverSpec extends munit.FunSuite:

  def assertBoardEquals(
      actual: Array[Array[Char]],
      expected: Array[Array[Char]]
  )(using munit.Location): Unit =
    assertEquals(
      actual.map(_.mkString).toSeq,
      expected.map(_.mkString).toSeq
    )

  test("solves standard puzzle"):
    val b = board(
      "53..7....",
      "6..195...",
      ".98....6.",
      "8...6...3",
      "4..8.3..1",
      "7...2...6",
      ".6....28.",
      "...419..5",
      "....8..79"
    )
    Solution.solveSudoku(b)
    assertBoardEquals(
      b,
      board(
        "534678912",
        "672195348",
        "198342567",
        "859761423",
        "426853791",
        "713924856",
        "961537284",
        "287419635",
        "345286179"
      )
    )

  test("solves board with one blank"):
    val b = board(
      "534678912",
      "672195348",
      "198342567",
      "859761423",
      "426853791",
      "713924856",
      "961537284",
      "287419635",
      "34528617." // last cell is '9'
    )
    Solution.solveSudoku(b)
    assertEquals(b(8)(8), '9')

  test("solves board with many blanks (hard puzzle)"):
    val b = board(
      "8........",
      "..36.....",
      ".7..9.2..",
      ".5...7...",
      "....457..",
      "...1...3.",
      "..1....68",
      "..85...1.",
      ".9....4.."
    )
    Solution.solveSudoku(b)
    // Verify completeness and validity rather than exact solution
    assert(b.flatten.forall(_ != '.'), "Board still contains blank cells")
    val converted = cellBoard(b.map(_.mkString)*)
    assert(Validation.validateBoard(converted).isRight, "Solved board failed validation")

  test("leaves already-solved board unchanged"):
    val b = board(
      "534678912",
      "672195348",
      "198342567",
      "859761423",
      "426853791",
      "713924856",
      "961537284",
      "287419635",
      "345286179"
    )
    val original = b.map(_.clone())
    Solution.solveSudoku(b)
    assertBoardEquals(b, original)

  test("solves board with all blanks — produces valid complete solution"):
    val b = board(
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    Solution.solveSudoku(b)
    // All cells filled
    assert(b.flatten.forall(_ != '.'), "Board still contains blank cells")
    // Result is valid
    val converted = cellBoard(b.map(_.mkString)*)
    assert(Validation.validateBoard(converted).isRight, "Solved board failed validation")
    // Each row, col, box contains all digits 1-9
    val digits = ('1' to '9').toSet
    b.foreach(row => assertEquals(row.toSet, digits, "Row missing digits"))
    b.transpose.foreach(col => assertEquals(col.toSet, digits, "Col missing digits"))

  test("throws IllegalArgumentException for invalid character"):
    val b = board(
      "X3..7....",
      "6..195...",
      ".98....6.",
      "8...6...3",
      "4..8.3..1",
      "7...2...6",
      ".6....28.",
      "...419..5",
      "....8..79"
    )
    intercept[IllegalArgumentException]:
      Solution.solveSudoku(b)

  test("throws IllegalArgumentException for duplicate in row"):
    val b = board(
      "55..7....",
      "6..195...",
      ".98....6.",
      "8...6...3",
      "4..8.3..1",
      "7...2...6",
      ".6....28.",
      "...419..5",
      "....8..79"
    )
    intercept[IllegalArgumentException]:
      Solution.solveSudoku(b)

  test("throws IllegalArgumentException for wrong board size"):
    val b = Array(Array('5', '3'), Array('6', '.'))
    intercept[IllegalArgumentException]:
      Solution.solveSudoku(b)

  test("throws IllegalStateException when no solution exists"):
    // Row 9 col 8 and col 9 are both constrained to need the same digit
    val b = board(
      "12345678.",
      "........9",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    intercept[IllegalStateException]:
      Solution.solveSudoku(b)
