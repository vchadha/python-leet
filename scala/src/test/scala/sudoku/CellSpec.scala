package sudoku

class CellSpec extends munit.FunSuite:

  // ── Cell ───────────────────────────────────────────────────────────────────
  test("Cell.fromChar converts all valid digits"):
    ('1' to '9').foreach { d =>
      assertEquals(Cell.fromChar(d), Some(Filled(d)), s"Expected Some(Filled('$d'))")
    }

  test("Cell.fromChar returns Some(Blank) for '.'"):
    assertEquals(Cell.fromChar('.'), Some(Blank))

  test("Cell.fromChar returns None for invalid characters"):
    List('0', 'A', ' ', '!').foreach { c =>
      assertEquals(Cell.fromChar(c), None, s"Expected None for '$c'")
    }

  // ── Cell Helpers ───────────────────────────────────────────────────────────
  test("CellHelpers.toFilledSet returns only Filled values when Blanks are mixed in"):
    val cells = List(Filled('1'), Blank, Filled('3'), Blank, Filled('5'))
    assertEquals(
      CellHelpers.toFilledSet(cells),
      Set(Filled('1'), Filled('3'), Filled('5'))
    )

  test("CellHelpers.toFilledSet returns all values when input contains only Filled"):
    val cells = List(Filled('1'), Filled('2'), Filled('3'))
    assertEquals(
      CellHelpers.toFilledSet(cells),
      Set(Filled('1'), Filled('2'), Filled('3'))
    )

  test("CellHelpers.toFilledSet returns empty set when input contains only Blanks"):
    val cells = List(Blank, Blank, Blank)
    assertEquals(CellHelpers.toFilledSet(cells), Set.empty[Filled])

  test("CellHelpers.toFilledSet returns empty set when empty set is input"):
    assertEquals(CellHelpers.toFilledSet(List()), Set.empty[Filled])

  test("CellHelpers.toFilledSet deduplicates repeated Filled values"):
    val cells = List(Filled('5'), Filled('5'), Filled('5'))
    assertEquals(CellHelpers.toFilledSet(cells), Set(Filled('5')))
