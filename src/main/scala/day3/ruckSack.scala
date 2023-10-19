import utils.readFile
import RuckSack.calculateWorth
import RuckSack.ruckSacksFromCharGroups
import RuckSack.calculateGroupWorth

case class RuckSack(chars: List[Char]):
  val first = chars.take(chars.size / 2)
  val second = chars.drop(chars.size / 2)

object RuckSack:

  def calculateWorth(ruckSack: RuckSack): Int =
    val commonChars = ruckSack.first.intersect(ruckSack.second)
    getValueOfCommon(commonChars)

  def calculateGroupWorth(ruckSacks: List[RuckSack]): Int =
    val commonChars = ruckSacks.map(_.chars).reduce(_ intersect _)
    getValueOfCommon(commonChars)

  def ruckSacksFromCharGroups(groups: List[List[Char]]): List[RuckSack] =
    groups.map(RuckSack.apply).toList

  private val lowerCaseValues = ('a' to 'z').zip(1 to 26).toMap
  private val upperCaseValues = ('A' to 'Z').zip(27 to 52).toMap
  private val values = lowerCaseValues ++ upperCaseValues

  private def getValueOfCommon(commonChars: List[Char]): Int =
    commonChars
      .groupMapReduce(c => c)(_ => +1)(_ + _)
      .keySet
      .intersect(values.keySet)
      .map(values)
      .sum

@main def mainDay3: Unit =
  val input = readFile("day3.txt")
    .getOrElse(throw Exception("Failed to read file"))
    .split("\n")
    .toList

  val answer1 =
    input
      .map(entry => RuckSack(entry.toCharArray().toList))
      .map(ruckSack => calculateWorth(ruckSack))
      .sum

  val answer2 =
    input
      .map(_.toCharArray().toList)
      .grouped(3)
      .map(ruckSacksFromCharGroups)
      .map(calculateGroupWorth)
      .sum

  println(answer1)
  println(answer2)
