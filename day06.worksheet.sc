import scala.util.matching.Regex
import scala.collection.GenSet
import scala.io.Source
import collection.JavaConverters._

def readDelimited(fileName: String, delimiter : String) : Iterator[String] = {
  asScalaIterator(new java.util.Scanner(new java.io.File(fileName)).useDelimiter(delimiter))
}

def solve1(fileName: String) = {
    readDelimited(fileName, "\n\n")
        .map(group => group
            .split("\n")
            .flatMap(_.split(""))
            .toSet
            .size
        )
        .foldLeft(0) { (a, v) => a + v }
}

// Part 1
solve1("day06.txt")

// Part 2
def toPeopleAndCharList(group: String) =
    (group.split("\n").length, group.replaceAll("\n", "").split("").toList)

def countWhereUniqueCharCountEqPeopleCount(people: Int, chars: List[String]) =
    chars.toSet.filter(char => chars.count(_ == char) == people).size

def solve2(fileName: String) = {
    readDelimited(fileName, "\n\n")
        .map(toPeopleAndCharList(_))
        .map(countWhereUniqueCharCountEqPeopleCount(_, _))
        .foldLeft(0) { (a, v) => a + v }
}

solve2("day06.txt")