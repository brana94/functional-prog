import org.junit.Test
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.scalatestplus.junit.JUnitSuite

class GridHelperSuite extends JUnitSuite {

  private val matIndexGen: Gen[(GridHelper.MatrixPos)] =
    for {
      x <- Gen.choose(0, 8)
      y <- Gen.choose(0, 8)
    } yield (x, y)

  private val middleMatIndexGen: Gen[(GridHelper.MatrixPos)] =
    for {
      x <- Gen.choose(1, 7)
      y <- Gen.choose(1, 7)
    } yield (x, y)

  private val arrIndexGen: Gen[Int] = Gen.choose(0, 80)

  private val matrixArrayMatrixProperty = forAll(matIndexGen) {
    pos => GridHelper.matrixIndex(GridHelper.arrayIndex(pos)) === pos
  }

  private val arrayMatrixArrayProperty = forAll(arrIndexGen) {
    pos => GridHelper.arrayIndex(GridHelper.matrixIndex(pos)) === pos
  }

  private val matrixArrayValidProperty = forAll {
    pos: (Int, Int) => {
      if (GridHelper.isValidIndex(pos))
        GridHelper.isValidIndex(GridHelper.arrayIndex(pos))
      else
        true
    }
  }

  private val validMatrixIndexProperty = forAll(matIndexGen) {
    pos => GridHelper.isValidIndex(pos)
  }

  private val validArrayIndexProperty = forAll(arrIndexGen) {
    pos => GridHelper.isValidIndex(pos)
  }

  private val upDownLeftRightProperty = forAll(middleMatIndexGen) {
    pos => GridHelper.right(GridHelper.left(GridHelper.down(GridHelper.up(pos)))) === pos
  }

  private val solvedArr =
    "386157492745923816291468753967241385153879264428635179519382647834796521672514938".toCharArray

  private val unsolvedArr =
    "38615-49274592381629146875396-2413851538-926----6351-95193-26478347-65216725149--".toCharArray

  private val unsolvableArr =
    "1234-6789----5-------------------------------------------------------------------".toCharArray

  @Test def matrixToArrayIndex(): Unit = {
    assert(GridHelper.arrayIndex((2, 4)) === 22)
  }

  @Test def arrayToMatrixIndex(): Unit = {
    assert(GridHelper.matrixIndex(22) === (2, 4))
  }

  @Test def validMatrixIndex(): Unit = {
    assert(GridHelper.isValidIndex((2, 3)))
  }

  @Test def validArrayIndex(): Unit = {
    assert(GridHelper.isValidIndex(45))
  }

  @Test def invalidMatrixIndex(): Unit = {
    assert(!GridHelper.isValidIndex((9, 5)))
  }

  @Test def invalidArrayIndex(): Unit = {
    assert(!GridHelper.isValidIndex(89))
  }

  @Test def matrixArrayMatrix(): Unit = matrixArrayMatrixProperty.check()
  @Test def arrayMatrixArray(): Unit = arrayMatrixArrayProperty.check()
  @Test def matrixArrayValid(): Unit = matrixArrayValidProperty.check()
  @Test def validArrayIdx(): Unit = validArrayIndexProperty.check()
  @Test def validMatrixIdx(): Unit = validMatrixIndexProperty.check()

  @Test def upAndLeft(): Unit = {
    assert(GridHelper.left(GridHelper.up((2, 5))) === (1, 4))
  }

  @Test def invalidDownAndRight(): Unit = {
    assert(GridHelper.right(GridHelper.down((8, 8))) === (8, 8))
  }

  @Test def upDownLeftRight(): Unit = upDownLeftRightProperty.check()

  @Test def solvedGrid(): Unit = {
    assert(GridHelper.isSolvedGrid(solvedArr))
  }

  @Test def unsolvedGrid(): Unit = {
    assert(!GridHelper.isSolvedGrid(unsolvedArr))
  }

  @Test def solveGrid(): Unit = {
    val solution = GridHelper.solveGrid(unsolvedArr).get
    assert(GridHelper.isSolvedGrid(solution))
    assert(solution === solvedArr)
  }

  @Test def unsolvableGrid(): Unit = {
    assert(GridHelper.solveGrid(unsolvableArr) === None)
  }

  @Test def validGrid(): Unit = {
    assert(GridHelper.isValidState(unsolvedArr))
  }

  @Test def invalidGrid(): Unit = {
    assert(!GridHelper.isValidGrid(new Grid(unsolvableArr).asArray()))
  }

  @Test def moveSequenceSolvesGrid(): Unit = {
    val initGrid = unsolvedArr.updated(67, 'P')
    val moveSeq = GridHelper.moveSequenceFromSolution(initGrid, solvedArr)
    var newGrid = initGrid
    var currentIdx = 67
    for (m <- moveSeq) {
      m match {
        case 'u' =>
          currentIdx = GridHelper.arrayIndex(GridHelper.up(GridHelper.matrixIndex(currentIdx)))
        case 'd' =>
          currentIdx = GridHelper.arrayIndex(GridHelper.down(GridHelper.matrixIndex(currentIdx)))
        case 'l' =>
          currentIdx = GridHelper.arrayIndex(GridHelper.left(GridHelper.matrixIndex(currentIdx)))
        case 'r' =>
          currentIdx = GridHelper.arrayIndex(GridHelper.right(GridHelper.matrixIndex(currentIdx)))
        case num =>
          newGrid = newGrid.updated(currentIdx, num)
      }
    }
    assert(newGrid === solvedArr)
  }
}
