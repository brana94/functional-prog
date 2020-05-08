import org.scalatestplus.junit.JUnitSuite
import org.junit.Test
import org.scalacheck.Gen
import org.scalacheck.Prop._

class GridSuite extends JUnitSuite {

  private val indexGen: Gen[(Int, Int)] =
    for {
      x <- Gen.choose(0, 8)
      y <- Gen.choose(0, 8)
    } yield (x, y)

  private val numGen: Gen[Char] =
    Gen.oneOf('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

  private def gridGen: Gen[Array[Char]] =
    Gen.containerOfN[Array, Char](81, Gen.oneOf('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-', 'P'))

  private val addRemoveProperty = forAll(gridGen, numGen, indexGen){
    (g, n, i) => {
      val initGrid = new Grid(g)
      initGrid.transform(Add(i, n)).transform(Remove(i)).grid == initGrid.grid
    }
  }

  private val transposeProperty = forAll(gridGen){
    g => {
      val initGrid = new Grid(g)
      initGrid.transform(Transpose()).transform(Transpose()) == initGrid
    }
  }

  private val exchangeProperty = forAll(gridGen){
    g => {
      val initGrid = new Grid(g)
      initGrid.transform(Exchange()).transform(Exchange()) == initGrid
    }
  }

  private val gridArr =
    "4-129--752--3--8-P-7--8---6---1-3-621-5---4-373-6-8---6---2--3---7--1--489--651--".toCharArray

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

  @Test def addAndRemove(): Unit = {
    addRemoveProperty.check()
  }

  // TODO ADD NORMAL START TEST

  // TODO ADD NORMAL TRANSPOSE TEST

  @Test def transpose(): Unit = {
    transposeProperty.check()
  }

  // TODO ADD NORMAL EXCHANGE TEST

  @Test def exchange(): Unit = {
    exchangeProperty.check()
  }

  // todo normal filter row col
  // todo normal filter square
  // todo chain add filter filter is always valid property
  // todo sequence add filter filter is always valid property
}
