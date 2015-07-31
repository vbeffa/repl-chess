/**
 * Chess game for the scala repl.
 */

import scala.language.postfixOps

object Color extends Enumeration {
  val Black, White = Value
}

case class File(file: Char) {
  if (file < 'a' || file > 'h') throw new IllegalArgumentException(s"Invalid file $file")
  override def toString = file.toString
}

object File {
  final val Files = Seq(a, b, c, d, e, f, g, h)
  object a extends File('a')
  object b extends File('b')
  object c extends File('c')
  object d extends File('d')
  object e extends File('e')
  object f extends File('f')
  object g extends File('g')
  object h extends File('h')
}

case class Rank(rank: Int) {
  if (rank < 1 || rank > 8) throw new IllegalArgumentException(s"Invalid rank $rank")
  override def toString = rank.toString
}

object Rank {
  final val Ranks = Seq(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`)
  object `1` extends Rank(1)
  object `2` extends Rank(2)
  object `3` extends Rank(3)
  object `4` extends Rank(4)
  object `5` extends Rank(5)
  object `6` extends Rank(6)
  object `7` extends Rank(7)
  object `8` extends Rank(8)
}

case class Position(file: File, rank: Rank)

case class Square(color: Color.Value) {
  import Color._
  
  def draw(line: Int) = {
    color match {
      case Black => print("@@@@@@@")
      case White => print("       ")
    }
  }
}

object Square {
  final val Width = 7
  final val PrintWidth = Width + 1
  final val MidWidth = Width / 2 + 1
  final val Height = 3
}

object BlackSquare extends Square(Color.Black)
object WhiteSquare extends Square(Color.White)

case class Row(rank: Rank) {
  import Rank._
  
  val squares = rank match {
    case `1` | `3` | `5` | `7` =>
      Seq(BlackSquare, WhiteSquare, BlackSquare, WhiteSquare, BlackSquare, WhiteSquare, BlackSquare, WhiteSquare)
    case `2` | `4` | `6` | `8` =>
      Seq(WhiteSquare, BlackSquare, WhiteSquare, BlackSquare, WhiteSquare, BlackSquare, WhiteSquare, BlackSquare)
  }
  
  def draw() = {
    import Square._
    import File._
    import Row._
    
    println("  " + "-" * PrintWidth * 8)
    Rows foreach { line =>
      print(s"""${if (line == MiddleRow) rank else " "} """)
      Files foreach { file =>
        print("|")
        squares(file.file - `a`.file).draw(line)
        if (file == `h`)
          print("|")
      }
      println()
    }
    if (rank == `1`) {
      println("  ----------------------------------------------------------------")
      print("   ")
      Files foreach (file => print(" " * (MidWidth - 1) + file + " " * MidWidth))
      println()
    }
  }
}

object Row {
  final val NumRows = 3
  final val Rows = 1 to NumRows
  final val MiddleRow = NumRows / 2 + 1
}

abstract class Piece(val color: Color.Value)

case class Pawn(override val color: Color.Value) extends Piece(color)

case class Knight(override val color: Color.Value) extends Piece(color)

case class Bishop(override val color: Color.Value) extends Piece(color)

case class Rook(override val color: Color.Value) extends Piece(color)

case class Queen(override val color: Color.Value) extends Piece(color)

case class King(override val color: Color.Value) extends Piece(color)

case class Move(from: Position, to: Position) {
  override def toString = s"${from.file.file}${from.rank.rank} ${to.file.file}${to.rank.rank}"
}

case class Board(moves: Seq[Move]) {
  import Rank._
  
  val squares = Ranks map Row.apply
  
  def draw() = squares.reverse foreach (_.draw())
}

object Board {  
  def apply(): Board = Board(Nil)
}

object Game {
  var board: Board = newGame()
  
  def newGame() = {
    val board = Board()
    board.draw()
    this.board = board
    board
  }
  
  object Positions {
    object a1 extends Position(File.a, Rank.`1`) {
      def a2 = {
        board = Board(board.moves :+ Move(Positions.a1, Positions.a2))
        board
      }
    }
    
    object a2 extends Position(File.a, Rank.`2`)
  }
  
  final val a1 = Positions.a1
}

import Game._
