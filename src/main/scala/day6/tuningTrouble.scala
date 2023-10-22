// scala-cli directives
//> using resourceDir ../../resources
//> using file ../utils/utilities.scala

import utils.readFile

@main def mainDay6: Unit =
  val input =
    readFile("day6.txt")
      .getOrElse(throw Exception("Could not read file"))
      .toCharArray()

  val sequence = input.view
    .sliding(size = 4)
    .find(group => group.toList.distinct.size == 4)
    .getOrElse(throw Exception("Did not find anything."))
    .force

  val position = input.view.sliding(4).indexWhere(a => a.toVector == sequence)

  println(sequence)
  println(position + 4) // we start at 1790, we need to go 4 more forward
