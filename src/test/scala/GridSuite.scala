import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.scalacheck.Gen
import org.scalacheck.Prop._

class GridSuite extends JUnitSuite {

  private val gridGen: Gen[Array[Char]] =
    Gen.containerOfN[Array, Char](81, Gen.oneOf('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', 'P'))

  /*private val indexGen: Gen[(GridHelper.MatrixPos)] =
    for {
      x <- Gen.choose(0, 8)
      y <- Gen.choose(0, 8)
    } yield (x, y)

  private val valGen: Gen[Char] = Gen.choose('1'.toChar, '9'.toChar)*/

  private val addRemoveProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      val idx = g.indexOf('-')
      if (idx != -1) {
        val matIdx = GridHelper.matrixIndex(idx)
        initGrid.transform(Add(matIdx, '5')).transform(Remove(matIdx)).grid === initGrid.grid
      } else
        true
    }
  }

  private val transposeTwiceProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      initGrid.transform(Transpose()).transform(Transpose()).grid === initGrid.grid
    }
  }

  private val transposeValidProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      if (GridHelper.isValidGrid(initGrid.asArray()))
        GridHelper.isValidGrid(initGrid.transform(Transpose()).asArray())
      else
        !GridHelper.isValidGrid(initGrid.transform(Transpose()).asArray())
    }
  }

  private val exchangeTwiceProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      initGrid.transform(Exchange()).transform(Exchange()).grid === initGrid.grid
    }
  }

  private val exchangeValidProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      if (GridHelper.isValidGrid(initGrid.asArray()))
        GridHelper.isValidGrid(initGrid.transform(Exchange()).asArray())
      else
        !GridHelper.isValidGrid(initGrid.transform(Exchange()).asArray())
    }
  }

  private val chainAddFilterValidProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      val chain = new UserOperations()
        .createChain(List(Add((5, 5), '5'), FilterRowCol(5, 5), FilterSquare(5, 5)))
      if (GridHelper.isValidGrid(initGrid.asArray()) && initGrid.initialPos != (5, 5))
        GridHelper.isValidGrid(initGrid.transform(chain).asArray())
      else
        !GridHelper.isValidGrid(initGrid.transform(chain).asArray())
    }
  }

  private val sequenceAddFilterValidProperty = forAll(gridGen) {
    g => {
      val initGrid = new Grid(g)
      val seq = new UserOperations()
        .createSequence(List(Add((5, 5), '5'), FilterRowCol(5, 5), FilterSquare(5, 5)))
      if (GridHelper.isValidGrid(initGrid.asArray()) && initGrid.initialPos != (5, 5))
        GridHelper.isValidGrid(initGrid.transform(seq).asArray())
      else
        !GridHelper.isValidGrid(initGrid.transform(seq).asArray())
    }
  }

  private val gridArr =
    "4-129--752--3--8-P-7--8---6---1-3-621-5---4-373-6-8---6---2--3---7--1--489--651--".toCharArray

  private val transposedArr =
    "42--176-8--7--3--91---5--7-23-1-6---9-8---2-6---3-8-15-8--4---17--6--3--5P623--4-".toCharArray

  private val exchangedArr =
    "6-981--358--7--2-P-3--2---4---9-7-489-5---6-737-4-2---4---8--7---3--9--621--459--".toCharArray

  private val unfilteredRowColArr =
    "555555555---5--------5--------5--------5--------5--------5--------5--------5-----".toCharArray

  private val unfilteredSquareArr =
    "---555------555------555---------------------------------------------------------".toCharArray

  private val filteredArr =
    "---5-----------------------------------------------------------------------------".toCharArray

  private val initialGrid = new Grid(gridArr)

  @Test def remove(): Unit = {
    val removed = initialGrid.transform(Remove(2, 4))
    assert(removed.grid(22) === '-')
    for (i <- 0 to 21) assert(initialGrid.grid(i) === removed.grid(i))
    for (i <- 23 to 80) assert(initialGrid.grid(i) === removed.grid(i))
  }

  @Test def add(): Unit = {
    val added = initialGrid.transform(Add((0, 1), '8'))
    assert(added.grid(1) === '8')
    assert(initialGrid.grid(0) === added.grid(0))
    for (i <- 2 to 80) assert(initialGrid.grid(i) === added.grid(i))
  }

  @Test def addRemove(): Unit = {
    addRemoveProperty.check()
  }

  @Test def start(): Unit = {
    val oldStartIdx = GridHelper.arrayIndex(initialGrid.initialPos)
    val changed = initialGrid.transform(Start(0, 0))
    assert(changed.grid(0) == 'P')
    assert(changed.grid(oldStartIdx) == '-')
    for (i <- 1 until oldStartIdx) assert(initialGrid.grid(i) === changed.grid(i))
    for (i <- oldStartIdx + 1 to 80) assert(initialGrid.grid(i) === changed.grid(i))
  }

  @Test def transpose(): Unit = {
    val transposed = initialGrid.transform(Transpose())
    assert(transposed.grid == transposedArr.toSeq)
  }

  @Test def transposeTwice(): Unit = {
    transposeTwiceProperty.check()
  }

  @Test def transposeValid(): Unit = {
    transposeValidProperty.check()
  }

  @Test def exchange(): Unit = {
    val exchanged = initialGrid.transform(Exchange())
    assert(exchanged.grid === exchangedArr)
  }

  @Test def exchangeTwice(): Unit = {
    exchangeTwiceProperty.check()
  }

  @Test def exchangeValid(): Unit = {
    exchangeValidProperty.check()
  }

  @Test def filterRowCol(): Unit = {
    val initGrid = new Grid(unfilteredRowColArr)
    assert(initGrid.transform(FilterRowCol(0, 3)).grid === filteredArr)
  }

  @Test def filterSquare(): Unit = {
    val initGrid = new Grid(unfilteredSquareArr)
    assert(initGrid.transform(FilterSquare(0, 3)).grid === filteredArr)
  }

  @Test def chainAddFilterValid(): Unit = {
    chainAddFilterValidProperty.check()
  }

  @Test def sequenceAddFilterValid(): Unit = {
    sequenceAddFilterValidProperty.check()
  }
}
