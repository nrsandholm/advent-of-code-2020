import scala.util.matching.Regex
import scala.collection.GenSet
import scala.io.Source
import collection.JavaConversions._

val required = Map[String, Regex](
    "byr" -> "^19[2-9][0-9]$|^200[0-2]$".r,
    "iyr" -> "^201[0-9]$|^2020$".r,
    "eyr" -> "^202[0-9]$|^2030$".r,
    "hgt" -> "^59in$|^6[0-9]in$|^7[0-6]in$|^1[5-8][0-9]cm$|^19[0-3]cm$".r,
    "hcl" -> "^#[0-9a-f]{6}$".r,
    "ecl" -> "^amb$|^blu$|^brn$|^gry$|^grn$|^hzl$|^oth$".r,
    "pid" -> "^[0-9]{9}$".r
)
val optional = Set("cid")

val eight = ("([a-z]{3}):([a-z0-9#]*)\\s" * 7 + "([a-z]{3}):([a-z0-9#]*)").r
val seven = ("([a-z]{3}):([a-z0-9#]*)\\s" * 6 + "([a-z]{3}):([a-z0-9#]*)").r

def readDelimited(fileName: String, delimiter : String) : Iterator[String] = {
  asScalaIterator(new java.util.Scanner(new java.io.File(fileName)).useDelimiter(delimiter))
}

def solve(fileName: String) = {
    readDelimited(fileName, "\n\n")
        .map {
            case eight(a1, a2, b1, b2, c1, c2, d1, d2, e1, e2, f1, f2, g1, g2, h1, h2) => Map(
                a1 -> a2,
                b1 -> b2,
                c1 -> c2,
                d1 -> d2,
                e1 -> e2,
                f1 -> f2,
                g1 -> g2,
                h1 -> h2
            )
            case seven(a1, a2, b1, b2, c1, c2, d1, d2, e1, e2, f1, f2, g1, g2) => Map(
                a1 -> a2,
                b1 -> b2,
                c1 -> c2,
                d1 -> d2,
                e1 -> e2,
                f1 -> f2,
                g1 -> g2
            )
            case _ => Map()
        }
        .filter {
            case map if map.size > 8 => false
            case map if map.size == 8 => map.forall { case (k, _) => required.containsKey(k) || optional.contains(k) }
            case map if map.size == 7 => map.forall { case (k, _) => required.containsKey(k) }
            case map if map.size < 7 => false
        }
}

// Part 1
solve("day04.txt").foldLeft(0) { (a, _) => a + 1 }

// Part 2
solve("day04.txt")
    .filter { map => map.forall { {
        case ("cid", v) => true 
        case (k, v) => required(k).findAllIn(v).length == 1
    } } }
    .foldLeft(0) { (a, _) => a + 1 }