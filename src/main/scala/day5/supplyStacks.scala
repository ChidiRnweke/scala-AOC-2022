// scala-cli directives
//> using resourceDir ../../resources
//> using file ../utils/utilities.scala
//> using file ../utils/state.scala

import utils.readFile
import utils.State

type StackState = Map[Stack, Vector[Item]]
type topItems = List[Item]

case class Item(str: String)
case class CrateMove(from: Stack, to: Stack, crates: Int)
enum Stack:
  case One, Two, Three, Four, Five, Six, Seven, Eight, Nine

object InputParser:
  private val pattern = """move (\d+) from (\d+) to (\d+)""".r
  private val stackMap =
    (1 to 9).map(i => i.toString -> Stack.values(i - 1)).toMap

  def parse(
      rawState: String,
      rawMoves: String
  ): Option[(StackState, List[CrateMove])] =
    parseShifts(rawMoves).map((convertToStackState(rawState), _))

  private def fromRawInput(input: String): Option[CrateMove] =
    input match
      case pattern(qty, from, to) =>
        Some(CrateMove(stackMap(from), stackMap(to), qty.toInt))
      case _ => None

  private def parseShifts(rawMoves: String): Option[List[CrateMove]] =
    val moves = rawMoves.split("\n").map(fromRawInput).toList
    if (moves.contains(None)) None else Some(moves.flatten)

  private def convertToStackState(input: String): StackState =
    val lines = input.split("\n").dropRight(1).reverse
    val stacks = Stack.values.toList

    val itemsPerStack = for
      line <- lines.toVector
      (entry, stack) <- line
        .grouped(4)
        .map(s => s.trim.stripPrefix("[").stripSuffix("]"))
        .zip(stacks)
      if entry.nonEmpty
    yield stack -> Item(entry)
    itemsPerStack.groupMap(_._1)(_._2)

def moveCrate(
    crateMove: CrateMove,
    reorderFn: Vector[Item] => Vector[Item]
): State[StackState, topItems] =
  for
    state <- State.get[StackState]
    updatedFromStack = state(crateMove.from).dropRight(crateMove.crates)
    to_items = state(crateMove.to) ++ reorderFn(
      state(crateMove.from).takeRight(crateMove.crates)
    )

    _ <- State.modify[StackState](s =>
      s.updated(crateMove.from, updatedFromStack)
        .updated(crateMove.to, to_items)
    )

    updatedState <- State.get[StackState]
  yield Stack.values.toList.flatMap(stack => updatedState(stack).takeRight(1))

def composeMoves(
    moves: List[CrateMove],
    reverse: Boolean
): State[StackState, topItems] =
  val strategy: Vector[Item] => Vector[Item] =
    if reverse then _.reverse else identity
  moves.map(moveCrate(_, strategy)).reduce(_ >> _)

@main def mainDay5: Unit =
  val (rawState, rawMoves) = readFile("day5.txt")
    .getOrElse(throw Exception("Failed to read file"))
    .split("\n\n")
    .toList
    .splitAt(1)

  val (start, moves) = InputParser
    .parse(rawState.mkString, rawMoves.mkString)
    .getOrElse(throw new Exception("Failed to parse input"))

  val resultString = composeMoves(moves, true).run(start)._1.map(_.str)
  val resultString2 = composeMoves(moves, false).run(start)._1.map(_.str)

  println(resultString.mkString)
  println(resultString2.mkString)
