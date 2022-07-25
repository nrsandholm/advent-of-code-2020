import javax.management.relation.Relation
import scala.io.Source

val bagWithoutContent = "([a-z ]*) bags contain no other bags\\.".r
val bagWithContent = "([a-z ]*) bags contain ([a-z1-9 ,\\.]*)".r
val bags = "([1-9] [a-z ]* bag[s]*[,\\.]*)".r
val bag = "([1-9]) ([a-z ]*) bag[s]*[,\\.]*".r

type Relations = List[(Int, String)]
type Bags = Map[String, Relations]

def toMap(map: Bags, list: Relations) = {
    list.foldLeft(map) {
        case (map, (_, bagg)) => if (map.contains(bagg)) then map else map + (bagg -> List())
    }
}

def parseBags(str: String): Relations = {
    bags.findAllIn(str).map {
        case bag(num, name) => (num.toInt, name)
    }.toList
}

def process(fileName: String) = {
    val bufferedSource = Source.fromFile(fileName)
    val result = bufferedSource
        .getLines()
        .foldLeft(Map[String, Relations]())((map, line) => line match {
            case bagWithoutContent(bag) => map + (bag -> List())
            case bagWithContent(bag, bags) => {
                val tuples = parseBags(bags)
                val m = toMap(map, tuples)
                m + (bag -> tuples)
            }
            case _ => throw new Error("not supposed to happen..")
        })
        
    bufferedSource.close
    result
}

val map = process("day07.txt")

// Part 1
// TODO @tailrec
def findBagsThatCanContainBag(bag: String): Set[String] =
    map.foldLeft(Set()) {
        case (set, (bagg, rel)) => 
            if (rel.find((_, baggg) => baggg == bag).isDefined) then
                (set + bagg) ++ findBagsThatCanContainBag(bagg)
            else
                set
    }

findBagsThatCanContainBag("shiny gold").size

// Part 2
// TODO @tailrec
def countBags(bag: String): Int =
    val rel = map(bag)
    rel.foldLeft(1) {
        case (sum, (num, bagg)) => sum + num * countBags(bagg)
    }

// Exclude outer most bag from count
countBags("shiny gold") - 1