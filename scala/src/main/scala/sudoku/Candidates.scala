package sudoku

/**
  * Sealed trait to represent candidates for a cell
  */
sealed trait Candidates
case object Empty                        extends Candidates
case class NonEmpty(values: Set[Filled]) extends Candidates

object Candidates {

  /**
    * Constructor.
    * Returns Empty if the set is blank.
    *
    * @param values Set of Filled values
    * @return If set is empty, return Empty, else NonEmpty containing input values
    */
  def apply(values: Set[Filled]): Candidates =
    if values.isEmpty then Empty
    else NonEmpty(values)

  /**
    * Remove a value, collapsing to Empty if needed
    *
    * @param candidates Candidates to remove from
    * @param value Value to remove
    * @return New set of candidates with value removed. Possibly empty.
    */
  def remove(candidates: Candidates, value: Filled): Candidates =
    candidates match
      case Empty            => Empty
      case NonEmpty(values) => Candidates(values - value)

  /**
    * Return size of candidates.
    *
    * @param candidates set of candidates to get size from
    * @return size of set. 0 if empty.
    */
  def size(candidates: Candidates): Int =
    candidates match
      case Empty            => 0
      case NonEmpty(values) => values.size

}
