// Has an immutable matrix of chars and an initial position.
class Grid(g: IndexedSeq[Char]) {
  val grid: IndexedSeq[Char] = g
  val initialPos: GridHelper.MatrixPos = GridHelper.matrixIndex(grid.indexOf('P'))

  // Returns a matrix representation of the grid.
  def asArray(): Array[Array[Char]] = grid.grouped(9).toArray.map(_.toArray)

  // Executes an operation to create a new grid.
  def transform(op: Operation): Grid = {
    op match {
      case Remove(pos) => new Grid(grid.updated(GridHelper.arrayIndex(pos), '-'))
      case Add(pos, n) if n.isDigit => new Grid(grid.updated(GridHelper.arrayIndex(pos), n))
      case Start(pos) =>
        new Grid(
          grid.updated(grid.indexOf('P'), '-')
              .updated(GridHelper.arrayIndex(pos), 'P')
        )
      case Chain(f) => f(this)
      case Transpose() => new Grid(grid.grouped(9).toIndexedSeq.transpose.flatten)
      case Exchange() => {
        var g = grid
        for (i <- 0 to 80) {
          g(i) match {
            case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
              g = g.updated(i, ('9' - g(i) + '1').toChar)
            case _ =>
          }
        }
        new Grid(g)
      }
      case FilterRowCol(pos) => {
        var g = grid
        val c = grid(GridHelper.arrayIndex(pos))
        for (i <- 0 to 8) {
          val rowIdx = GridHelper.arrayIndex((pos._1, i))
          val colIdx = GridHelper.arrayIndex((i, pos._2))
          if (i != pos._2 && g(rowIdx) == c) g = g.updated(rowIdx, '-')
          if (i != pos._1 && g(colIdx) == c) g = g.updated(colIdx, '-')
        }
        new Grid(g)
      }
      case FilterSquare(pos) => {
        val arrPos = GridHelper.arrayIndex(pos)
        val arrStart = GridHelper.arrayIndex((pos._1 / 3, pos._2 / 3))
        var newGrid = grid
        for (i <- arrStart to arrStart + 2) {
          if (i != arrPos && newGrid(i) == newGrid(arrPos)) {
            newGrid = newGrid.updated(i, '-')
          }
        }
        for (i <- arrStart + 9 to arrStart + 11) {
          if (i != arrPos && newGrid(i) == newGrid(arrPos)) {
            newGrid = newGrid.updated(i, '-')
          }
        }
        for (i <- arrStart + 18 to arrStart + 20) {
          if (i != arrPos && newGrid(i) == newGrid(arrPos)) {
            newGrid = newGrid.updated(i, '-')
          }
        }
        new Grid(newGrid)
      }
      case Sequence(list) => {
        var g = this
        for (o <- list) g = g.transform(o)
        g
      }
    }
  }
}
