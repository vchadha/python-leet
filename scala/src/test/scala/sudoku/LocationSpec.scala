package sudoku

class LocationSpec extends munit.FunSuite:

  // ── Location ───────────────────────────────────────────────────────────────
  test("Location.isPeerOf: same row is peer"):
    assert(Location(0, 0).isPeerOf(Location(0, 8)))

  test("Location.isPeerOf: same col is peer"):
    assert(Location(0, 0).isPeerOf(Location(8, 0)))

  test("Location.isPeerOf: same box is peer"):
    assert(Location(0, 0).isPeerOf(Location(2, 2)))

  test("Location.isPeerOf: different row, col, and box is not peer"):
    assert(!Location(0, 0).isPeerOf(Location(4, 4)))

  test("Location.isPeerOf: is symmetric"):
    val a = Location(0, 0)
    val b = Location(0, 5)
    assertEquals(a.isPeerOf(b), b.isPeerOf(a))

  test("Location.subBoxIndex: each box has exactly one representative — all 9 boxes"):
    // One cell from each box to confirm every box index is reachable (spot check)
    assertEquals(Location(0, 0).subBoxIndex, 0) // top-left box
    assertEquals(Location(0, 3).subBoxIndex, 1) // top-middle box
    assertEquals(Location(0, 6).subBoxIndex, 2) // top-right box
    assertEquals(Location(3, 0).subBoxIndex, 3) // middle-left box
    assertEquals(Location(3, 3).subBoxIndex, 4) // center box
    assertEquals(Location(3, 6).subBoxIndex, 5) // middle-right box
    assertEquals(Location(6, 0).subBoxIndex, 6) // bottom-left box
    assertEquals(Location(6, 3).subBoxIndex, 7) // bottom-middle box
    assertEquals(Location(6, 6).subBoxIndex, 8) // bottom-right box

  test("Location.subBoxIndex: row boundaries — rows 2 and 3 are in different boxes"):
    assertEquals(Location(2, 0).subBoxIndex, 0) // last row of box 0
    assertEquals(Location(3, 0).subBoxIndex, 3) // first row of box 3
    assertEquals(Location(5, 0).subBoxIndex, 3) // last row of box 3
    assertEquals(Location(6, 0).subBoxIndex, 6) // first row of box 6

  test("Location.subBoxIndex: col boundaries — cols 2 and 3 are in different boxes"):
    assertEquals(Location(0, 2).subBoxIndex, 0) // last col of box 0
    assertEquals(Location(0, 3).subBoxIndex, 1) // first col of box 1
    assertEquals(Location(0, 5).subBoxIndex, 1) // last col of box 1
    assertEquals(Location(0, 6).subBoxIndex, 2) // first col of box 2

  test("Location.subBoxIndex: all 9 cells in box 0 map to index 0"):
    val box0 = for
      row <- 0 to 2
      col <- 0 to 2
    yield Location(row, col)
    assert(box0.forall(_.subBoxIndex == 0))

  test("Location.subBoxIndex: all 9 cells in box 4 (center) map to index 4"):
    val box4 = for
      row <- 3 to 5
      col <- 3 to 5
    yield Location(row, col)
    assert(box4.forall(_.subBoxIndex == 4))

  test("Location.subBoxIndex: all 9 cells in box 8 (bottom-right) map to index 8"):
    val box8 = for
      row <- 6 to 8
      col <- 6 to 8
    yield Location(row, col)
    assert(box8.forall(_.subBoxIndex == 8))

  test("Location.subBoxIndex: all 81 cells map to a valid box index in 0..8"):
    val allLocations = for
      row <- 0 to 8
      col <- 0 to 8
    yield Location(row, col)
    assert(allLocations.forall(loc => loc.subBoxIndex >= 0 && loc.subBoxIndex <= 8))

  test("Location.subBoxIndex: all 81 cells produce exactly 9 cells per box"):
    val allLocations = for
      row <- 0 to 8
      col <- 0 to 8
    yield Location(row, col)
    val grouped = allLocations.groupBy(_.subBoxIndex)
    assertEquals(grouped.size, 9)              // exactly 9 distinct boxes
    assert(grouped.values.forall(_.size == 9)) // exactly 9 cells each
