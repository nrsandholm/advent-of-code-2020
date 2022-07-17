import scala.io.Source

case class State(row: Int, col: Int, count: Int)

def solve(fileName: String, traverse: (State, String) => State) = {
    val bufferedSource = Source.fromFile(fileName)
    val result = bufferedSource
        .getLines()
        .foldLeft(State(0, 0, 0))(traverse)
        .count
    bufferedSource.close
    println(s"result: $result")
    result
}

def getTraverseFunc(row: Int, col: Int): (State, String) => State = {
    def f(state: State, line: String): State = {
        if (state.row % row != 0) return state.copy(state.row + 1)
        val isTree = line.charAt(state.col % line.length).equals('#')
        val count = if (isTree) state.count + 1 else state.count
        State(state.row + 1, state.col + col, count)
    }
    f
}

// Part 1
val a = solve("day03.txt", getTraverseFunc(1, 3))

// Part 2
val b = solve("day03.txt", getTraverseFunc(1, 1))

val c = solve("day03.txt", getTraverseFunc(1, 5))

val d = solve("day03.txt", getTraverseFunc(1, 7))

val e = solve("day03.txt", getTraverseFunc(2, 1))

a * b * c * d * e.toLong