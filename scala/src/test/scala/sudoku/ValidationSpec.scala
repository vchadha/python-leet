package sudoku

import sudoku.TestHelpers.{board, cellBoard}

class ValidationSpec extends munit.FunSuite:

  test("convertBoard: converts valid board successfully"):
    val result = Validation.convertBoard(
      board(
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
    )
    assert(result.isRight)

  test("convertBoard: '.' becomes Blank"):
    val result = Validation.convertBoard(
      board(
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
    )
    result match
      case Left(_)      => fail("Expected Right")
      case Right(board) => assert(board.flatten.forall(_ == Blank))

  test("convertBoard: digit chars become Filled"):
    val result = Validation.convertBoard(
      board(
        "123456789",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    result match
      case Left(_)      => fail("Expected Right")
      case Right(board) =>
        assertEquals(board(0)(0), Filled('1'))
        assertEquals(board(0)(8), Filled('9'))

  test("convertBoard: returns InvalidCell error with position for bad character"):
    val result = Validation.convertBoard(
      board(
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
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.InvalidCell(0, 0, 'X') => true
      case _                                      => false
    })

  test("convertBoard: accumulates all invalid character errors"):
    val result = Validation.convertBoard(
      board(
        "X........",
        "Z........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assertEquals(errors.length, 2)

  test("convertBoard: '0' is invalid"):
    val result = Validation.convertBoard(
      board(
        "0........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    assert(result.isLeft)

  test("validateBoard: accepts all-blank board"):
    val result = Validation.validateBoard(
      cellBoard(
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
    )
    assert(result.isRight)

  test("validateBoard: accepts fully solved board"):
    val result = Validation.validateBoard(
      cellBoard(
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
    assert(result.isRight)

  test("validateBoard: rejects wrong number of rows"):
    val result = Validation.validateBoard(
      Array(
        Array[Cell](Blank, Blank),
        Array[Cell](Blank, Blank)
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.contains(ValidationError.InvalidBoardSize))

  test("validateBoard: rejects rows of wrong length"):
    val shortRow = Array.fill(5)(Blank: Cell)
    val board    = Array.fill(9)(shortRow)
    val result   = Validation.validateBoard(board)
    val errors   = result.left.getOrElse(fail("Expected Left"))
    assert(errors.contains(ValidationError.InvalidBoardSize))

  test("validateBoard: detects duplicate in row — correct row index reported"):
    val result = Validation.validateBoard(
      cellBoard(
        "55.......",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.DuplicateInRow(0, '5') => true
      case _                                      => false
    })

  test("validateBoard: detects duplicate in column — correct col index reported"):
    val result = Validation.validateBoard(
      cellBoard(
        "5........",
        "5........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.DuplicateInCol(0, '5') => true
      case _                                      => false
    })

  test("validateBoard: detects duplicate in box — correct box index reported"):
    val result = Validation.validateBoard(
      cellBoard(
        "51.......",
        "1........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.DuplicateInBox(0, '1') => true
      case _                                      => false
    })

  test("validateBoard: accumulates errors across rows, cols, and boxes"):
    val result = Validation.validateBoard(
      cellBoard(
        "55.......", // dup in row 0, box 0
        "5........", // dup in col 0
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.length > 1, s"Expected multiple errors, got: ${errors.map(_.message)}")

  test("validateBoard: duplicate in last row reported with correct index"):
    val result = Validation.validateBoard(
      cellBoard(
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "55......."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.DuplicateInRow(8, '5') => true
      case _                                      => false
    })

  test("validateBoard: duplicate in last col reported with correct index"):
    val result = Validation.validateBoard(
      cellBoard(
        "........5",
        "........5",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.DuplicateInCol(8, '5') => true
      case _                                      => false
    })

  test("validateBoard: duplicate in last box reported with correct index"):
    val result = Validation.validateBoard(
      cellBoard(
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        ".........",
        "......51.",
        "......1..",
        "........."
      )
    )
    val errors = result.left.getOrElse(fail("Expected Left"))
    assert(errors.exists {
      case ValidationError.DuplicateInBox(8, '1') => true
      case _                                      => false
    })
