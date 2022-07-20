import scala.io.Source

def findRow(list: List[String], row: Int = 0, blockSize: Int = 128): (Int, Int) = list match {
    case c :: rest if c == "F" => findRow(rest, row, blockSize / 2)
    case c :: rest if c == "B" => findRow(rest, row + blockSize / 2, blockSize / 2)
    case _ => (row, findColumn(list))
}

def findColumn(list: List[String], column: Int = 0, blockSize: Int = 8): Int = list match {
    case f :: rest if f == "L" => findColumn(rest, column, blockSize / 2)
    case f :: rest if f == "R" => findColumn(rest, column + blockSize / 2, blockSize / 2)
    case Nil => column
}

// Part 1
def calcSeatID(tuple: (Int, Int)) = tuple._1 * 8 + tuple._2

def solve1(fileName: String) = {
    val bufferedSource = Source.fromFile(fileName)
    val result = bufferedSource
        .getLines()
        .map(str => calcSeatID(findRow(str.split("").toList)))
        .foldLeft(-1) { (a, b) => if (b > a) b else a }
    bufferedSource.close
    println(s"result: $result")
    result
}

solve1("day05.txt")

// Part 2
def solve2(fileName: String) = {
    val bufferedSource = Source.fromFile(fileName)
    val result = bufferedSource
        .getLines()
        .map(str => findRow(str.split("").toList))
        .toList
        .groupBy(_._1) // Group columns (seats) by row
        .toList
        .filter(_._2.length == 7) // Find row(s) missing one
        .flatMap(_._2)
        .foldRight((0, Range(0, 8).sum)) { (value, acc) => (value._1, acc._2 - value._2)} // Find non-existing (row, column)
    bufferedSource.close
    println(s"result: $result")
    result
}

val tuple = solve2("day05.txt")
calcSeatID(tuple)