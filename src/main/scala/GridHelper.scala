object GridHelper {
  type MatrixPos = (Int, Int)

  def matrixIndex(arrayIdx: Int): MatrixPos = (arrayIdx / 9, arrayIdx % 9)
  def arrayIndex(matrixIdx: MatrixPos): Int = matrixIdx._1 * 9 + matrixIdx._2

  def isValidIndex(arrayIdx: Int): Boolean = arrayIdx >= 0 && arrayIdx <= 80
  def isValidIndex(matrixIdx: MatrixPos): Boolean =
    matrixIdx._1 >= 0 && matrixIdx._1 <= 8 && matrixIdx._2 >= 0 && matrixIdx._2 <= 8

  def newPos(oldIdx: MatrixPos)(move: MatrixPos => MatrixPos): MatrixPos = {
    val newIdx = move(oldIdx)
    if (isValidIndex(newIdx)) newIdx else oldIdx
  }

  def up(matrixIdx: MatrixPos): MatrixPos = newPos(matrixIdx)(pos => (pos._1 - 1, pos._2))
  def down(matrixIdx: MatrixPos): MatrixPos = newPos(matrixIdx)(pos => (pos._1 + 1, pos._2))
  def left(matrixIdx: MatrixPos): MatrixPos = newPos(matrixIdx)(pos => (pos._1, pos._2 - 1))
  def right(matrixIdx: MatrixPos): MatrixPos = newPos(matrixIdx)(pos => (pos._1, pos._2 + 1))

  def takeSquare(origGrid: IndexedSeq[Char], idx: Int): IndexedSeq[Char] = {
    val xStart = math.floor(idx / 3).toInt * 3
    val yStart = (idx % 3) * 3
    val startIdx = arrayIndex((xStart, yStart))
    origGrid.slice(startIdx, startIdx + 3)
      .appendedAll(origGrid.slice(startIdx + 9, startIdx + 12))
      .appendedAll(origGrid.slice(startIdx + 18, startIdx + 21))
  }

  def takeRow(origGrid: IndexedSeq[Char], idx: Int): IndexedSeq[Char] =
    origGrid.slice(idx * 9, idx * 9 + 9)

  def takeCol(origGrid: IndexedSeq[Char], idx: Int): IndexedSeq[Char] =
    for (i <- 0 to 8) yield origGrid(i * 9 + idx)

  def isSolvedGrid(g: IndexedSeq[Char]): Boolean =
    !g.contains('P') && !g.contains('-') && isValidState(g)

  def solveGrid(g: IndexedSeq[Char]): Option[IndexedSeq[Char]] = {
    // Check whether the current state is valid.
    // If it isn't, it can't lead to a solution.
    if (!isValidState(g)) return None

    // Check whether the gris is already solved.
    // If it is, a solution has been reached.
    if (isSolvedGrid(g)) return Some(g)

    // Try all possible values for the first empty position.
    // Return the first one that yields a solution, if there is one.
    for (c <- '1' to '9') {
      val indexP = if (g.contains('P')) g.indexOf('P') else 80
      val indexEmpty = if (g.contains('-')) g.indexOf('-') else 80
      val firstEmpty = math.min(indexP, indexEmpty)
      val res = solveGrid(g.updated(firstEmpty, c))
      if (res.isDefined) return res
    }

    // No solution found.
    None
  }

  def solveGrid(g: Grid): Option[IndexedSeq[Char]] = solveGrid(g.grid)

  // Checks the validity of a sudoku unit (row, column, square).
  // Verifies that the same number does not appear twice.
  def isValidUnit(arr: IndexedSeq[Char]): Boolean =
    !{ for (i <- 1 to 9) yield arr.count(c => c == (i + '0').toChar) }.exists(_ > 1)

  // Checks the validity of a state in a sudoku game.
  def isValidState(g: IndexedSeq[Char]): Boolean = {
    for (i <- 0 to 8) {
      if (!isValidUnit(takeSquare(g, i)))
        return false
      if (!isValidUnit(takeRow(g, i)))
        return false
      if (!isValidUnit(takeCol(g, i)))
        return false
    }
    true
  }

  // Checks the validity of a sudoku grid in grid form.
  // No violations of rules (row, col, square).
  // Is solvable.
  def isValidGrid(g: Array[Array[Char]]): Boolean = {
    // Check that grid dimensions are 9x9.
    if (g.length != 9) return false
    if (!g.map(arr => arr.length == 9).reduce((b1, b2) => b1 && b2)) return false

    val flat = g.flatten
    val allowedCharacters = Set('1', '2', '3', '4', '5', '6', '7', '8', '9', '-', 'P')

    // Check that the grid has no invalid characters and that it has exactly one 'P' symbol.
    if (!flat.forall(allowedCharacters.contains)) return false
    if (flat.count(_ == 'P') != 1) return false

    // Check that no sudoku rules are violated by the grid.
    if (!isValidState(flat.toIndexedSeq)) return false

    // Finally, check that the grid can be solved.
    solveGrid(flat.toIndexedSeq).isDefined
  }

  // Returns a sequence of moves that transforms the initial grid into the solved grid.
  def moveSequenceFromSolution(init: IndexedSeq[Char], sol: IndexedSeq[Char]): List[Char] = {
    var moveList = List.empty[Char]
    val initPos = matrixIndex(init.indexOf('P'))

    // Move from initial position to (0, 0).
    for (_ <- 0 until initPos._1) moveList = moveList.appended('u')
    for (_ <- 0 until initPos._2) moveList = moveList.appended('l')

    // Go through the grid, filling in all the empty fields with the correct values.
    for (i <- 0 to 80) {
      if (init(i) != sol(i)) moveList = moveList.appended(sol(i))
      if (i != 80) {
        if (i % 9 == 8) {
          // Go one step down and 8 steps left to get to the start of the next row.
          moveList = moveList.appended('d')
          for (_ <- 0 until 8) moveList = moveList.appended('l')
        } else {
          // Go one step to the right.
          moveList = moveList.appended('r')
        }
      }
    }

    moveList
  }

  def moveSequenceFromSolution(initGrid: Grid, solution: IndexedSeq[Char]): List[Char] =
    moveSequenceFromSolution(initGrid.grid, solution)
}
