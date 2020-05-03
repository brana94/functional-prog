sealed abstract class Operation
case class Remove(pos: GridHelper.MatrixPos) extends Operation
case class Add(pos: GridHelper.MatrixPos, num: Char) extends Operation
case class Start(pos: GridHelper.MatrixPos) extends Operation
case class Transpose() extends Operation
case class Exchange() extends Operation
case class FilterRowCol(pos: GridHelper.MatrixPos) extends Operation
case class FilterSquare(pos: GridHelper.MatrixPos) extends Operation
case class Chain(f: Grid => Grid) extends Operation
case class Sequence(list: List[Operation]) extends Operation