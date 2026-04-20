package sudoku

class CandidatesSpec extends munit.FunSuite:

  // ── Candidates ─────────────────────────────────────────────────────────────
  test("Candidates.apply returns Empty for empty set"):
    assertEquals(Candidates(Set.empty[Filled]), Empty)

  test("Candidates.apply returns NonEmpty for non-empty set"):
    assertEquals(
      Candidates(Set(Filled('1'), Filled('2'))),
      NonEmpty(Set(Filled('1'), Filled('2')))
    )

  test("Candidates.remove removed value from list with size greater than 1"):
    assertEquals(
      Candidates.remove(NonEmpty(Set(Filled('5'), Filled('6'))), Filled('5')),
      NonEmpty(Set(Filled('6')))
    )

  test("Candidates.remove collapses to Empty when last value removed"):
    assertEquals(
      Candidates.remove(NonEmpty(Set(Filled('5'))), Filled('5')),
      Empty
    )

  test("Candidates.remove from Empty stays Empty"):
    assertEquals(Candidates.remove(Empty, Filled('3')), Empty)

  test("Candidates.size returns 0 for Empty"):
    assertEquals(Candidates.size(Empty), 0)

  test("Candidates.size returns element count for NonEmpty"):
    assertEquals(Candidates.size(NonEmpty(Set(Filled('1'), Filled('3'), Filled('7')))), 3)
