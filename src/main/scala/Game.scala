import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.{readChar, readInt, readLine}

object Game {
  val savedGrids: UserGrids = new UserGrids()
  val savedOperations: UserOperations = new UserOperations()

  // The grid, current state and position are uninitialized in the beginning.
  // A game can't be started until it is read from a file.
  var initialGrid: Option[Grid] = None
  var currentState: Option[IndexedSeq[Char]] = None
  var currentPos: Option[(Int, Int)] = None

  // Ignores all other characters.
  // Returns whether the operation was successful.
  private def executeMove(m: Char): Boolean = {
    def tryWrite(c: Char): Boolean = {
      val curArrayIdx = GridHelper.arrayIndex(currentPos.get)
      val initChar = initialGrid.get.grid(curArrayIdx)
      if (initChar == 'P' || initChar == '-')
      { currentState = Some(currentState.get.updated(curArrayIdx, c)); true }
      else false
    }

    val oldPos = currentPos.get
    m match {
      case 'd' =>
        currentPos = Some(GridHelper.down(currentPos.get))
        !currentPos.get.equals(oldPos)

      case 'u' =>
        currentPos = Some(GridHelper.up(currentPos.get))
        !currentPos.get.equals(oldPos)

      case 'l' =>
        currentPos = Some(GridHelper.left(currentPos.get))
        !currentPos.get.equals(oldPos)

      case 'r' =>
        currentPos = Some(GridHelper.right(currentPos.get))
        !currentPos.get.equals(oldPos)

      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => tryWrite(m)
    }
  }

  // Executes a sequence of moves.
  // Returns whether the whole sequence was successful.
  private def executeSequence(seq: List[Char]): Boolean =
    { for (m <- seq) yield executeMove(m) }.reduce((b1, b2) => b1 && b2)

  // Creates a simple operation to transform an existing grid.
  private def createSimpleOperation(opName: String): Option[Operation] = {
    def readPos(): Option[(Int, Int)] = {
      println("Unesite broj reda.")
      val x = readInt()
      println("Unesite broj kolone.")
      val y = readInt()
      if (x >= 0 && x <= 8 && y >= 0 && y <= 8) Some((x, y))
      else {
        println("Greska! Red i/ili kolona su van opsega.")
        None
      }
    }

    opName match {
      case "ukloni" | "start" =>
        val pos = readPos()
        if (pos.isDefined) {
          if (opName.equals("ukloni"))
            Some(Remove(pos.get))
          else
            Some(Start(pos.get))
        } else None

      case "dodaj" =>
        val pos = readPos()
        if (pos.isDefined) {
          println("Unesite broj koji zelite da dodate.")
          val v = readInt()
          if (v >= 1 && v <= 9) {
            Some(Add(pos.get, (v + '0').toChar))
          } else {
            println("Greska! Broj nije validan."); None
          }
        } else None

      case "transpozicija" => Some(Transpose())

      case "zamena" => Some(Exchange())

      case "filtriranje" | "filtriranje kvadrata" =>
        val pos = readPos()
        if (pos.isDefined) {
          if (opName.equals("filtriranje"))
            Some(FilterRowCol(pos.get))
          else
            Some(FilterSquare(pos.get))
        } else None

      case opName =>
        val ret = savedOperations.getOperation(opName)
        if (ret.isEmpty)
          println("Trazena operacija ne postoji.")
        ret
    }
  }

  // Creates a list of operations for creating a chain or sequence.
  @tailrec
  private def createOperationList(): List[Operation] = {
    var opList = List.empty[Operation]

    println("Unesite naziv prve operacije:")
    printAllOperations()

    var opName = readLine()
    val op = createSimpleOperation(opName)

    if (op.isDefined) {
      println()
      println("Unesite naziv sledece operacije, ili napisite 'kraj' da zavrsite:")
      printAllOperations()

      opName = readLine()

      while (!opName.equals("kraj")) {
        val op = createSimpleOperation(opName)

        if (op.isDefined) {
          opList = opList.appended(op.get)
        } else {
          println("Operacija nije validna, pokusajte ponovo.")
        }

        println()
        println("Unesite naziv sledece operacije, ili napisite 'kraj' da zavrsite:")
        printAllOperations()

        opName = readLine()
      }

      opList
    } else {
      println("Operacija nije validna, pokusajte ponovo.")
      createOperationList()
    }
  }

  // Checks whether the current state is valid and whether the user has solved the game.
  private def checkState(): Unit = {
    if (currentState.isEmpty)
      return

    // Check for errors in the current state.
    if (!GridHelper.isValidState(currentState.get)) {
      println("Greska! Stanje tabele nije validno.")
      return
    }

    // If there are no errors, check whether the game is finished.
    // If it is, print a message and reset the game.
    if (GridHelper.isSolvedGrid(currentState.get)) {
      println("Bravo! Uspesno ste resili sudoku!")
      currentState = None
      currentPos = None
      initialGrid = None
    }
  }

  // Prints a list of all currently available operations.
  private def printAllOperations(): Unit =
    for (op <- savedOperations.operationList()) println(s"- $op")

  // Print the current state to make the application more user-friendly.
  private def printState(): Unit = {
    if (currentState.isDefined) {
      println("Trenutno stanje tabele:")
      currentState.get.grouped(9).foreach(arr => {
        for (i <- 0 to 8) {
          print(s"${arr(i)} ")
        }
        println()
      })
      println(s"Trenutna pozicija: ${currentPos.get}")
    }
  }

  // Game functionalities.

  def newGridFromFile(): Unit = {
    println("Unesite ime fajla iz kojeg zelite da ucitate tabelu.")
    val fileName = readLine()
    println("Unesite ime za novu tabelu.")
    val gridName = readLine()
    savedGrids.gridFromFile(gridName, fileName)
  }

  def newGame(): Unit = {
    println("Unesite ime tabele.")
    val gridName = readLine()

    val res = savedGrids.getGame(gridName)
    res match {
      case None => println("Tabela nije pronadjena.")
      case Some(t) =>
        println("Igra je uspesno ucitana.")
        initialGrid = Some(t._1); currentState = Some(t._2); currentPos = Some(t._3)
        printState()
    }
  }

  @tailrec
  def move(): Unit = {
    if (currentPos.isEmpty) {
      println("Greska! Ne moze da se odigra potez dok se ne zapocne igra.")
    } else {
      println("Uneti zeljeni potez:")
      println("d. dole")
      println("u. gore")
      println("l. levo")
      println("r. desno")
      println("1. upisati 1")
      println("2. upisati 2")
      println("3. upisati 3")
      println("4. upisati 4")
      println("5. upisati 5")
      println("6. upisati 6")
      println("7. upisati 7")
      println("8. upisati 8")
      println("9. upisati 9")

      val allowedMoves = Set('d', 'u', 'l', 'r', '1', '2', '3', '4', '5', '6', '7', '8', '9')

      val m = readChar()
      if (allowedMoves.contains(m)) {
        val oldPos = currentPos.get
        val ok = executeMove(m)

        // Check whether the operation was valid.
        if (!ok) {
          if (m.isLetter)
            println(s"Potez ${oldPos} -> ${currentPos.get} nije validan!")
          if (m.isDigit)
            println("Nije dozvoljen upis preko brojeva iz originalne tabele!")
        }
      } else {
        println("Uneti potez ne postoji. Pokusajte ponovo."); move()
      }
    }
  }

  def moveSequence(): Unit = {
    if (currentState.isEmpty) {
      println("Greska! Ne moze da se odigra sekvenca poteza ako nije zapoceta igra.")
      return
    }

    println("Unesite ime fajla iz kojeg zelite da ucitate sekvencu poteza.")
    val fileName = readLine()

    val source =  try Source.fromFile(fileName, "UTF-8")
      catch { case e: Throwable => println("Fajl nije pronadjen."); return }

    val lines: List[String] = try source.getLines().toList map {_.stripLineEnd}
      catch { case e: Throwable => println("Dogodila se greska prilikom citanja fajla."); List.empty[String] }
      finally source.close()

    if (!lines.forall(l => l.length == 1)) {
      println("Greska! Tacno jedna operacija mora da se pojavi u svakom redu.")
    } else {
      val success = executeSequence(lines.map(l => l(0)))

      if (success)
        println("Svi potezi su uspesno izvrseni!")
      else
        println("Greska! Neki od poteza iz sekvence nisu validni. Svi validni potezi su izvrseni.")
    }
  }

  def newGridFromExisting(): Unit = {
    println("Unesite ime tabele koju zelite da modifikujete.")
    val gridName = readLine()

    // Fetch the old grid.
    val oldGrid = savedGrids.getGrid(gridName)
    if (oldGrid.isEmpty) {
      println("Greska! Trazena tabela ne postoji.")
      return
    }

    println("Unesite ime operacije koju zelite da izvrsite.")
    printAllOperations()
    val opName = readLine()

    // Fetch the operation.
    val op = createSimpleOperation(opName)

    if (op.isEmpty) {
      println("Greska! Operacija nije mogla da se kreira.")
      return
    }

    val newGrid = oldGrid.get.transform(op.get)

    println("Unesite naziv za novu tabelu.")
    val name = readLine()
    savedGrids.saveGrid(name, newGrid)
  }

  def createComplexOperation(): Unit = {
    val opList = createOperationList()

    println("Kakvu slozenu operaciju zelite da kreirate?")
    println("1. lanac")
    println("2. sekvencu")
    var t = readChar()
    println()

    while (t != '1' && t != '2') {
      println("Greska! Uneti broj nije validan. Pokusajte ponovo.")
      t = readChar()
      println()
    }

    val op = t match {
      case '1' => savedOperations.createChain(opList)
      case '2' => savedOperations.createSequence(opList)
    }

    println("Kako zelite da nazovete operaciju?")
    val opName = readLine()

    savedOperations.saveOperation(opName, op)
  }

  def solveGrid(): Unit = {
    println("Unesite naziv tabele koju zelite da resite.")
    val gridName = readLine()
    val gridToSolve = savedGrids.getGrid(gridName)

    if (gridToSolve.isEmpty) {
      println("Greska! Trazena tabela ne postoji.")
      return
    }

    val solution = GridHelper.solveGrid(gridToSolve.get)
    if (solution.isEmpty) {
      // This should never happen, since an unsolvable grid can't be saved.
      println("Greska! Ova tabela je neresiva.")
      return
    }

    // Generate the string to write to the solution file.
    val toWrite =
      GridHelper.moveSequenceFromSolution(gridToSolve.get, solution.get).mkString("\n")

    // Open the solution file and write the solution to it.
    val file = new File(s"src/main/resources/${gridName}_sol.txt")
    val pw = new PrintWriter(file)
    pw.write(toWrite)
    pw.close()

    println(s"Resenje je upisano u datoteku ${file.getPath}.")
  }

  def prompt(): Unit = {
    println()
    println("Odaberite operaciju:")
    println("1. Ucitavanje sudoku tabele iz fajla")
    println("2. Pocetak nove igre")
    println("3. Novi potez")
    println("4. Sekvenca poteza")
    println("5. Pravljenje nove tabele od postojece")
    println("6. Kreiranje nove slozene operacije")
    println("7. Ispis resenja igre")
    println("0. Zavrsetak rada sa aplikacijom")
  }

  @tailrec
  def executeOperation(): Unit = {
    val choice = readChar()
    choice match {
      case '0' => println("Hvala na koriscenju."); System.exit(0)
      case '1' => newGridFromFile()
      case '2' => newGame()
      case '3' => move(); printState(); checkState()
      case '4' => moveSequence(); printState(); checkState()
      case '5' => newGridFromExisting()
      case '6' => createComplexOperation()
      case '7' => solveGrid()
      case _ => println("Uneti broj nije validan. Pokusajte ponovo."); executeOperation()
    }
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      prompt()
      executeOperation()
      // Small pause before next prompt.
      Thread.sleep(1000)
    }
  }
}