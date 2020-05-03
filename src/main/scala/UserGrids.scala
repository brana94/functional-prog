import scala.io.Source

class UserGrids {
  var grids: Map[String, Grid] = Map.empty[String, Grid]

  def gridFromFile(gridName: String, fileName: String): Unit = {
    val source =  try Source.fromFile(fileName, "UTF-8")
    catch { case e: Throwable => println("Fajl nije pronadjen."); return }
    val lines: List[String] = try source.getLines().toList map {_.stripLineEnd}
    catch { case e: Throwable => println("Dogodila se greska prilikom citanja fajla."); List.empty[String] }
    finally source.close()

    saveGrid(gridName, lines.map(_.toCharArray).toArray)
  }

  def getGrid(gridName: String): Option[Grid] = grids.get(gridName)

  def getGame(gridName: String): Option[(Grid, IndexedSeq[Char], GridHelper.MatrixPos)] = {
    val initGrid = grids.get(gridName)
    initGrid match {
      case None => None
      case Some(g) => Some(g, g.grid, g.initialPos)
    }
  }

  def saveGrid(gridName: String, g: Array[Array[Char]]): Unit = {
    if (GridHelper.isValidGrid(g)) {
      grids = grids.updated(gridName, new Grid(g.flatten.toIndexedSeq))
      println("Tabela je uspesno sacuvana.")
    }
    else println("Tabela nije validna, ne moze da se sacuva.")
  }

  def saveGrid(gridName: String, g: Grid): Unit = saveGrid(gridName, g.asArray())
}