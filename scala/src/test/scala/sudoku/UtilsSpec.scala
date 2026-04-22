package sudoku

import sudoku.TestHelpers.cellBoard

class UtilsSpec extends munit.FunSuite:

  val standardBoard = cellBoard(
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

  // ── getSubBoxCells structure ──────────────────────────────────────────────

  test("getSubBoxCells: returns exactly 9 subboxes"):
    assertEquals(Utils.getSubBoxCells(standardBoard).length, 9)

  test("getSubBoxCells: each subbox contains exactly 9 cells"):
    Utils.getSubBoxCells(standardBoard).zipWithIndex.foreach { case (box, i) =>
      assertEquals(box.length, 9, s"Box $i has ${box.length} cells, expected 9")
    }

  test("getSubBoxCells: works on all-blank board"):
    val blank = cellBoard(
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
    val boxes = Utils.getSubBoxCells(blank)
    assertEquals(boxes.length, 9)
    boxes.foreach(box => assert(box.forall(_ == Blank)))

  // ── getSubBoxCells contents ───────────────────────────────────────────────

  test("getSubBoxCells: box 0 contains top-left 3x3 cells"):
    val boxes = Utils.getSubBoxCells(standardBoard)
    // standardBoard top-left box: 5,3,4 / 6,7,2 / 1,9,8
    assertEquals(
      boxes(0).toSet[Cell],
      Set[Cell](
        Filled('5'),
        Filled('3'),
        Filled('4'),
        Filled('6'),
        Filled('7'),
        Filled('2'),
        Filled('1'),
        Filled('9'),
        Filled('8')
      )
    )

  test("getSubBoxCells: box 4 contains center 3x3 cells"):
    val boxes = Utils.getSubBoxCells(standardBoard)
    // standardBoard center box rows 3-5, cols 3-5: 7,6,1 / 8,5,3 / 9,2,4
    assertEquals(
      boxes(4).toSet[Cell],
      Set[Cell](
        Filled('7'),
        Filled('6'),
        Filled('1'),
        Filled('8'),
        Filled('5'),
        Filled('3'),
        Filled('9'),
        Filled('2'),
        Filled('4')
      )
    )

  test("getSubBoxCells: box 8 contains bottom-right 3x3 cells"):
    val boxes = Utils.getSubBoxCells(standardBoard)
    // standardBoard bottom-right box rows 6-8, cols 6-8: 2,8,4 / 6,3,5 / 1,7,9
    assertEquals(
      boxes(8).toSet[Cell],
      Set[Cell](
        Filled('2'),
        Filled('8'),
        Filled('4'),
        Filled('6'),
        Filled('3'),
        Filled('5'),
        Filled('1'),
        Filled('7'),
        Filled('9')
      )
    )

  test("getSubBoxCells: box ordering is row-major (left-to-right, top-to-bottom)"):
    // Use a board where each box is uniquely identifiable by its first cell
    val board = cellBoard(
      "123......",
      ".........",
      ".........",
      "...456...",
      ".........",
      ".........",
      "......789",
      ".........",
      "........."
    )
    val boxes = Utils.getSubBoxCells(board)
    // Box 0 contains '1','2','3' (and blanks)
    assert(boxes(0).contains(Filled('1')))
    assert(boxes(0).contains(Filled('2')))
    assert(boxes(0).contains(Filled('3')))
    // Box 4 contains '4','5','6'
    assert(boxes(4).contains(Filled('4')))
    assert(boxes(4).contains(Filled('5')))
    assert(boxes(4).contains(Filled('6')))
    // Box 8 contains '7','8','9'
    assert(boxes(8).contains(Filled('7')))
    assert(boxes(8).contains(Filled('8')))
    assert(boxes(8).contains(Filled('9')))
