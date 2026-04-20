package sudoku

class ValidationErrorSpec extends munit.FunSuite:

  // ── ValidationError messages ───────────────────────────────────────────────
  test("ValidationError.InvalidBoardSize has correct message"):
    assertEquals(ValidationError.InvalidBoardSize.message, "Board must be 9x9")

  test("ValidationError.InvalidCell has correct message"):
    assertEquals(
      ValidationError.InvalidCell(3, 7, 'Z').message,
      "Invalid character 'Z' at (3, 7)"
    )

  test("ValidationError.DuplicateInRow has correct message"):
    assertEquals(ValidationError.DuplicateInRow(2, '4').message, "Duplicate '4' in row 2")

  test("ValidationError.DuplicateInCol has correct message"):
    assertEquals(ValidationError.DuplicateInCol(5, '9').message, "Duplicate '9' in col 5")

  test("ValidationError.DuplicateInBox has correct message"):
    assertEquals(ValidationError.DuplicateInBox(8, '1').message, "Duplicate '1' in box 8")
