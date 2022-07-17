import scala.io.Source

val line = "(\\d*)-(\\d*) ([a-zA-Z]{1}): ([\\w]*)".r

def solve(fileName: String, isValid: (Int, Int, Char, String) => Boolean) = {
    val bufferedSource = Source.fromFile(fileName)
    val result = bufferedSource
        .getLines()
        .map {
            case line(no1, no2, char, password) => isValid(no1.toInt, no2.toInt, char.charAt(0), password)
        }
        .foldLeft(0) { (a, b) => if (b) a + 1 else a }
    println(s"result: $result")
    bufferedSource.close
}

// Part 1
def isValidPolicy1(no1: Int, no2: Int, char: Char, password: String): Boolean = {
    val count = password.count(_ == char)
    count >= no1 && count <= no2
}

solve("./day02.txt", isValidPolicy1)

// Part 2
def isValidPolicy2(no1: Int, no2: Int, char: Char, password: String): Boolean = {
    val charAt1 = password.charAt(no1 - 1)
    val charAt2 = password.charAt(no2 - 1)
    (charAt1 == char && charAt2 != char) || (charAt2 == char && charAt1 != char)
}

solve("./day02.txt", isValidPolicy2)