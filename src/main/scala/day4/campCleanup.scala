// scala-cli directives
//> using resourceDir ../../resources
//> using file ../utils/utilities.scala

import utils.readFile
import Section.fromShiftList

case class Section(start: Int, end: Int):

  def containsOther(that: Section): Boolean =
    (this.start >= that.start && this.end <= that.end)
      || (this.start <= that.start && this.end >= that.end)

  def hasOverlap(that: Section): Boolean =
    this.start <= that.end && this.end >= that.start

object Section:
  def fromShiftList(shifts: List[String]): Option[(Section, Section)] =
    val splitShifts = shifts.map(s => s.split("-").toList)
    splitShifts match
      case List(first, second) =>
        for
          firstShift <- fromRawInput(first)
          secondShift <- fromRawInput(second)
        yield (firstShift, secondShift)
      case _ => None

  def fromRawInput(input: List[String]): Option[Section] =
    input match
      case List(start, end) => Some(Section(start.toInt, end.toInt))
      case _                => None

@main def mainDay4: Unit =
  val input = readFile("day4.txt")
    .getOrElse(throw Exception("Failed to read file"))
    .split("\n")
    .map(l => l.split(",").toList)
    .toList
    .map(a => fromShiftList(a).getOrElse(throw Exception("Invalid input")))

  val answer1 = input
    .map((a, b) => a.containsOther(b))
    .filter(_ == true)
    .size

  val answer2 = input
    .map((a, b) => a.hasOverlap(b))
    .filter(_ == true)
    .size

  println(answer1)
  println(answer2)
