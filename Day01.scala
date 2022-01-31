object Day01 {
  def main(args: Array[String]) {
    if (args(0) == "silver") solveSilver(args(1))
    if (args(0) == "gold") solveGold(args(1))
  }

  def solveSilver(fileName: String) {
    val lines = scala.io.Source.fromFile(fileName).getLines
    val (numA, numB) = find2LinesThatSumUpTo(lines.toArray, 2020)
    val result = numA * numB
    println(result)
  }

  def find2LinesThatSumUpTo(lines: Array[String], target: Integer): (Integer, Integer) = {
    val linesWithIndex = lines.zipWithIndex
    for ((line1, index1) <- linesWithIndex) {
      for ((line2, index2) <- linesWithIndex) {
        if (index2 > index1) {
          val num1: Integer = line1.toInt
          val num2: Integer = line2.toInt
          if (num1 + num2 == target) {
              return (num1, num2)
          }
        }
      }
    }
    return (0, 0)
  }

  def solveGold(fileName: String) {
    val lines = scala.io.Source.fromFile(fileName).getLines
    val (num1, num2, num3) = find3LinesThatSumUpTo(lines.toArray, 2020)
    val result = num1 * num2 * num3
    println(result)
  }

  def find3LinesThatSumUpTo(lines: Array[String], target: Integer): (Integer, Integer, Integer) = {
    val linesWithIndex = lines.zipWithIndex
    for ((line1, index1) <- linesWithIndex) {
      for ((line2, index2) <- linesWithIndex) {
        for ((line3, index3) <- linesWithIndex) {
          if (index2 > index1 && index3 > index2) {
            val num1: Integer = line1.toInt
            val num2: Integer = line2.toInt
            val num3: Integer = line3.toInt
            if (num1 + num2 + num3 == target) {
                return (num1, num2, num3)
            }
          }
        }
      }
    }
    return (0, 0, 0)
  }
}
