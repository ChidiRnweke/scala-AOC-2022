import Move.*
import RawInput.*
import RawOppInput.*
import utils.readFile

enum RawOppInput:
  case A, B, C

object RawOppInput:
  def fromString(s: String): Option[RawOppInput] = s match
    case "A" => Some(A)
    case "B" => Some(B)
    case "C" => Some(C)
    case _   => None

enum RawInput:
  case X, Y, Z

object RawInput:
  def fromString(s: String): Option[RawInput] = s match
    case "X" => Some(X)
    case "Y" => Some(Y)
    case "Z" => Some(Z)
    case _   => None

enum Move(val score: Int):
  case Rock extends Move(1)
  case Paper extends Move(2)
  case Scissors extends Move(3)

enum Outcome(val score: Int):
  case Loss extends Outcome(0)
  case Draw extends Outcome(3)
  case Win extends Outcome(6)

object Move:
  def getOpponentMove(input: RawOppInput): Move =
    input match
      case RawOppInput.A => Rock
      case RawOppInput.B => Paper
      case RawOppInput.C => Scissors

  def getIdealMove(input: RawInput): Move =
    input match
      case RawInput.X => Rock
      case RawInput.Y => Paper
      case RawInput.Z => Scissors

  def winningMove(opponent: Move): Move =
    opponent match
      case Rock     => Paper
      case Paper    => Scissors
      case Scissors => Rock

  def losingMove(opponent: Move): Move =
    opponent match
      case Rock     => Scissors
      case Paper    => Rock
      case Scissors => Paper

  def getDesiredResponse(opp: RawOppInput, input: RawInput): Move =
    val opponentMove = getOpponentMove(opp)
    input match
      case RawInput.X => losingMove(opponentMove)
      case RawInput.Y => opponentMove
      case RawInput.Z => winningMove(opponentMove)

object Outcome:
  def getOutcome(you: Move, opponent: Move): Outcome =
    (you, opponent) match
      case (Rock, Rock) | (Paper, Paper) | (Scissors, Scissors) => Draw
      case (Rock, Scissors) | (Scissors, Paper) | (Paper, Rock) => Win
      case (Scissors, Rock) | (Paper, Scissors) | (Rock, Paper) => Loss

def countGamePoints(you: Move, opponent: Move): Int =
  Outcome.getOutcome(you, opponent).score + you.score

def inputToMoves(rawOppMv: RawOppInput, rawIdealMv: RawInput): (Move, Move) =
  (getOpponentMove(rawOppMv), getIdealMove(rawIdealMv))

def inputToDesiredMoves(rawOpp: RawOppInput, rawIdeal: RawInput): (Move, Move) =
  (getOpponentMove(rawOpp), getDesiredResponse(rawOpp, rawIdeal))

def lineToPair(line: String): Option[(RawOppInput, RawInput)] =
  line.split(" ") match
    case Array(opp, move) =>
      for
        rawOpponentMove <- RawOppInput.fromString(opp)
        rawMove <- RawInput.fromString(move)
      yield (rawOpponentMove, rawMove)
    case _ => None

@main def mainDay2: Unit =
  val moves =
    readFile("day2/input.txt")
      .getOrElse(throw Exception("Failed to read file"))
      .split("\n")
      .map(lineToPair)
      .map(_.getOrElse(throw Exception("Invalid input received")))

  val points = for
    (rawOp, rawMov) <- moves
    opp = getOpponentMove(rawOp)
    mov = getIdealMove(rawMov)
  yield countGamePoints(mov, opp)

  val pointsPart2 = for
    (rawOp, rawMov) <- moves
    opp = getOpponentMove(rawOp)
    mov = getDesiredResponse(rawOp, rawMov)
  yield countGamePoints(mov, opp)

  println(points.sum) // part 1 answer
  println(pointsPart2.sum) // part 2 answer
