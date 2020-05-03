class UserOperations {
  var operations: Map[String, Operation] = Map.empty[String, Operation]
  val predef: List[String] = List("ukloni", "dodaj", "start", "transpozicija",
                                  "zamena", "filtriranje", "filtriranje kvadrata")

  // Returns a list of all currently defined operations.
  def operationList(): List[String] = predef.appendedAll(operations.keys)

  def getOperation(opName: String): Option[Operation] = operations.get(opName)

  def saveOperation(opName: String, op: Operation): Unit =
    operations = operations.updated(opName, op)

  def createSequence(opList: List[Operation]): Operation = Sequence(opList)

  def createChain(opList: List[Operation]): Operation = {
    // Chains a list of functions into a single function.
    def compose(fList: List[Grid => Grid]): Grid => Grid = {
      fList match {
        case List() => (g : Grid) => g
        case List(x) => x
        case x :: xs => (g: Grid) => compose(xs)(x(g))
      }
    }

    Chain(compose(opList.map(op => (g : Grid) => g.transform(op))))
  }
}