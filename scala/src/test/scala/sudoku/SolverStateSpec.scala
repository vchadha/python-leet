package sudoku

import sudoku.TestHelpers.cellBoard

class SolverStateSpec extends munit.FunSuite:

  test("SolverState.from: identifies all blank cell locations"):
    val board = cellBoard(
      "53..7....",
      "6..195...",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    val state = SolverState.from(board)
    assert(state.blankCells.contains(Location(0, 2)))
    assert(state.blankCells.contains(Location(0, 3)))
    assert(state.blankCells.contains(Location(0, 5)))

  test("SolverState.from: no blank cells when board is fully solved"):
    val board = cellBoard(
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
    val state = SolverState.from(board)
    assert(state.blankCells.isEmpty, "Expected no blank cells in solved board")

  test("SolverState.from: all cells are blank when board is empty"):
    val board = cellBoard(
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
    val state = SolverState.from(board)
    assertEquals(state.blankCells.length, 81)

  test("SolverState.from: blank cell count matches actual blanks on board"):
    val board = cellBoard(
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
    val expectedBlanks = board.flatten.count(_ == Blank)
    val state          = SolverState.from(board)
    assertEquals(state.blankCells.length, expectedBlanks)

  test("SolverState.from: pre-filled cells are not in blankCells"):
    val board = cellBoard(
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
    val state = SolverState.from(board)
    // '5' at (0,0) should not be a blank
    assert(!state.blankCells.contains(Location(0, 0)))
    // '3' at (0,1) should not be a blank
    assert(!state.blankCells.contains(Location(0, 1)))

  test("SolverState.from: candidates map contains entry for every blank cell"):
    val board = cellBoard(
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
    val state = SolverState.from(board)
    state.blankCells.foreach { loc =>
      assert(
        state.candidates.contains(loc),
        s"Missing candidates entry for blank at $loc"
      )
    }

  test("SolverState.from: candidates map contains no entry for filled cells"):
    val board = cellBoard(
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
    val state = SolverState.from(board)
    // (0,0) is '5' — should not have candidates
    assert(!state.candidates.contains(Location(0, 0)))

  test("SolverState.from: candidates exclude digits already in same row"):
    // Row 0 has 5,3,7 — blank at (0,2) cannot be any of those
    val board = cellBoard(
      "53..7....",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    val state = SolverState.from(board)
    state.candidates(Location(0, 2)) match
      case Empty            => fail("Expected NonEmpty candidates")
      case NonEmpty(values) =>
        assert(!values.contains(Filled('5')), "5 already in row — should be excluded")
        assert(!values.contains(Filled('3')), "3 already in row — should be excluded")
        assert(!values.contains(Filled('7')), "7 already in row — should be excluded")

  test("SolverState.from: candidates exclude digits already in same col"):
    // Col 0 has 5,6 — blank at (2,0) cannot be either
    val board = cellBoard(
      "5........",
      "6........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    val state = SolverState.from(board)
    state.candidates(Location(2, 0)) match
      case Empty            => fail("Expected NonEmpty candidates")
      case NonEmpty(values) =>
        assert(!values.contains(Filled('5')), "5 already in col — should be excluded")
        assert(!values.contains(Filled('6')), "6 already in col — should be excluded")

  test("SolverState.from: candidates exclude digits already in same box"):
    // Box 0 has 5,3 — blank at (1,1) cannot be either
    val board = cellBoard(
      "53.......",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    val state = SolverState.from(board)
    state.candidates(Location(1, 1)) match
      case Empty            => fail("Expected NonEmpty candidates")
      case NonEmpty(values) =>
        assert(!values.contains(Filled('5')), "5 already in box — should be excluded")
        assert(!values.contains(Filled('3')), "3 already in box — should be excluded")

  test("SolverState.from: cell with only one candidate is NonEmpty with one value"):
    // Force (0,2) to have only one possible value
    val board = cellBoard(
      "12345678.", // only 9 is missing
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      ".........",
      "........."
    )
    val state = SolverState.from(board)
    assertEquals(
      state.candidates(Location(0, 8)),
      NonEmpty(Set(Filled('9')))
    )

  test("SolverState.from: empty board has all 9 digits as candidates for every cell"):
    val board = cellBoard(
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
    val state = SolverState.from(board)
    state.candidates.values.foreach {
      case Empty            => fail("Expected NonEmpty candidates on empty board")
      case NonEmpty(values) =>
        assertEquals(values, Filled.validFilled, "Expected all 9 digits as candidates")
    }
