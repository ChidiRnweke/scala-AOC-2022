import scala.math.max
import utils.readFile

case class Elf(items: List[Int]):
  val capacity = items.sum

def inputToElves(inputString: String): List[Elf] =
  val blocks = inputString.split("\n\n").toList
  val itemLists = blocks.map(block => block.split("\n").toList.map(_.toInt))
  itemLists.map(Elf(_))

@main def mainDay1: Unit =
  val input =
    readFile("day1/input.txt").getOrElse(throw Exception("Failed to read file"))

  val elves = inputToElves(input)
  val sortedCapacities = elves.map(_.capacity).sorted(Ordering[Int].reverse)

  val maxCap = sortedCapacities.head
  val topThree = sortedCapacities.take(3).sum

  println(maxCap)
  println(topThree)
