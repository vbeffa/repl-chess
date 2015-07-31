/**
 * Chess game for the scala repl.
 */

import scala.language.postfixOps

object Dimensions {
  final val InnerWidth = 7
  final val Width = InnerWidth + 1
  final val MidWidth = Width / 2
  final val Height = 3
}

object Color extends Enumeration {
  val Black = Value(1, "@")
  val White = Value(2, " ")
}

case class File(file: Char) {
  if (file < 'a' || file > 'h') throw new IllegalArgumentException(s"Invalid file $file")
  override def toString = file.toString
  def isRight = file == 'h'
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

object Position {
  final val a1 = Position(File.a, Rank.`1`)
  final val a2 = Position(File.a, Rank.`2`)
  final val a3 = Position(File.a, Rank.`3`)
  final val a4 = Position(File.a, Rank.`4`)
  final val a5 = Position(File.a, Rank.`5`)
  final val a6 = Position(File.a, Rank.`6`)
  final val a7 = Position(File.a, Rank.`7`)
  final val a8 = Position(File.a, Rank.`8`)
  final val b1 = Position(File.b, Rank.`1`)
  final val b2 = Position(File.b, Rank.`2`)
  final val b3 = Position(File.b, Rank.`3`)
  final val b4 = Position(File.b, Rank.`4`)
  final val b5 = Position(File.b, Rank.`5`)
  final val b6 = Position(File.b, Rank.`6`)
  final val b7 = Position(File.b, Rank.`7`)
  final val b8 = Position(File.b, Rank.`8`)
  final val c1 = Position(File.c, Rank.`1`)
  final val c2 = Position(File.c, Rank.`2`)
  final val c3 = Position(File.c, Rank.`3`)
  final val c4 = Position(File.c, Rank.`4`)
  final val c5 = Position(File.c, Rank.`5`)
  final val c6 = Position(File.c, Rank.`6`)
  final val c7 = Position(File.c, Rank.`7`)
  final val c8 = Position(File.c, Rank.`8`)
  final val d1 = Position(File.d, Rank.`1`)
  final val d2 = Position(File.d, Rank.`2`)
  final val d3 = Position(File.d, Rank.`3`)
  final val d4 = Position(File.d, Rank.`4`)
  final val d5 = Position(File.d, Rank.`5`)
  final val d6 = Position(File.d, Rank.`6`)
  final val d7 = Position(File.d, Rank.`7`)
  final val d8 = Position(File.d, Rank.`8`)
  final val e1 = Position(File.e, Rank.`1`)
  final val e2 = Position(File.e, Rank.`2`)
  final val e3 = Position(File.e, Rank.`3`)
  final val e4 = Position(File.e, Rank.`4`)
  final val e5 = Position(File.e, Rank.`5`)
  final val e6 = Position(File.e, Rank.`6`)
  final val e7 = Position(File.e, Rank.`7`)
  final val e8 = Position(File.e, Rank.`8`)
  final val f1 = Position(File.f, Rank.`1`)
  final val f2 = Position(File.f, Rank.`2`)
  final val f3 = Position(File.f, Rank.`3`)
  final val f4 = Position(File.f, Rank.`4`)
  final val f5 = Position(File.f, Rank.`5`)
  final val f6 = Position(File.f, Rank.`6`)
  final val f7 = Position(File.f, Rank.`7`)
  final val f8 = Position(File.f, Rank.`8`)
  final val g1 = Position(File.g, Rank.`1`)
  final val g2 = Position(File.g, Rank.`2`)
  final val g3 = Position(File.g, Rank.`3`)
  final val g4 = Position(File.g, Rank.`4`)
  final val g5 = Position(File.g, Rank.`5`)
  final val g6 = Position(File.g, Rank.`6`)
  final val g7 = Position(File.g, Rank.`7`)
  final val g8 = Position(File.g, Rank.`8`)
  final val h1 = Position(File.h, Rank.`1`)
  final val h2 = Position(File.h, Rank.`2`)
  final val h3 = Position(File.h, Rank.`3`)
  final val h4 = Position(File.h, Rank.`4`)
  final val h5 = Position(File.h, Rank.`5`)
  final val h6 = Position(File.h, Rank.`6`)
  final val h7 = Position(File.h, Rank.`7`)
  final val h8 = Position(File.h, Rank.`8`)
}

case class Square(color: Color.Value)

object Square {  
  object Black extends Square(Color.Black)
  object White extends Square(Color.White)
}

case class Row(rank: Rank) {
  import Rank._
  import Square._
  
  val row = rank match {
    case `1` | `3` | `5` | `7` =>
      Seq(Black, White, Black, White, Black, White, Black, White)
    case `2` | `4` | `6` | `8` =>
      Seq(White, Black, White, Black, White, Black, White, Black)
  }
  
  def isBottom = rank == `1`
}

object Row {
  import Dimensions._
  final val Lines = 1 to Height
  final val MiddleLine = Height / 2 + 1
}

case class Line(line: Int) {
  import Row._
  
  def isMiddle = line == MiddleLine
}

abstract class Piece(val color: Color.Value)

case class Pawn(override val color: Color.Value) extends Piece(color) {
  override def toString = "P"
}
object Pawn {
  object White extends Pawn(Color.White)
  object Black extends Pawn(Color.Black)
}

case class Knight(override val color: Color.Value) extends Piece(color) {
  override def toString = "N"
}
object Knight {
  object White extends Knight(Color.White)
  object Black extends Knight(Color.Black)
}

case class Bishop(override val color: Color.Value) extends Piece(color) {
  override def toString = "B"
}
object Bishop {
  object White extends Bishop(Color.White)
  object Black extends Bishop(Color.Black)
}

case class Rook(override val color: Color.Value) extends Piece(color) {
  override def toString = "R"
}
object Rook {
  object White extends Rook(Color.White)
  object Black extends Rook(Color.Black)
}

case class Queen(override val color: Color.Value) extends Piece(color) {
  override def toString = "Q"
}
object Queen {
  object White extends Queen(Color.White)
  object Black extends Queen(Color.Black)
}

case class King(override val color: Color.Value) extends Piece(color) {
  override def toString = "K"
}
object King {
  object White extends King(Color.White)
  object Black extends King(Color.Black)
}

case class Move(from: Position, to: Position) {
  override def toString = s"${from.file.file}${from.rank.rank} ${to.file.file}${to.rank.rank}"
}

case class Board(moves: Seq[Move], pieces: Map[Position,Piece]) {
  import Rank._
  
  val rows = Ranks map Row.apply
  
  def makeMove(move: Move): Board = Board(moves :+ move, (pieces - move.from) ++ Map(move.to -> pieces(move.from)))
  
  def pieceAt(pos: Position): Option[Piece] = pieces.get(pos)
}

object Renderer {
  def render(board: Board) = {
    
    def render(row: Row) = {
      import Dimensions._
      import File._
      import Row._
      
      def printSeparator() = println("  " + "-" * Width * 8)
      def printTop() = printSeparator()
      def printBottom() = printSeparator()
      
      def printFilesAtBottom() = {
        print("   ")
        Files foreach (file => print(" " * (MidWidth - 1) + file + " " * MidWidth))
        println()
      }
      
      def printLine(line: Line) = {
        def printRankOrSpaceAtLeft() = print(s"""${if (line.isMiddle) row.rank else " "} """)
        
        def printLineOfFile(file: File) = {
          val square = row.row(file.file - `a`.file)
          print("|")
          val colorChar = square.color.toString
          val piece = board.pieces.get(Position(file, row.rank)) map (_.toString) getOrElse colorChar
          print(colorChar * (MidWidth - 1))
          print(if (line.isMiddle) piece else colorChar)
          print(colorChar * (MidWidth - 1))
          if (file.isRight) print("|")
        }
        
        printRankOrSpaceAtLeft()
        Files foreach printLineOfFile
        println()
      }
      
      def printLines() = Lines map Line foreach printLine
      
      printTop()
      printLines()
      if (row.isBottom) {
        printBottom()
        printFilesAtBottom()
      }
    }
  
    board.rows.reverse foreach render
  }
}

val InitialPieces = {
  import Position._
  Map[Position,Piece](
    a8 -> Rook.Black, b8 -> Knight.Black, c8 -> Bishop.Black, d8 -> Queen.Black, e8 -> King.Black, f8 -> Bishop.Black, g8 -> Knight.Black, h8 -> Rook.Black,
    a7 -> Pawn.Black, b7 ->   Pawn.Black, c7 ->   Pawn.Black, d7 ->  Pawn.Black, e7 -> Pawn.Black, f7 ->   Pawn.Black, g7 ->   Pawn.Black, h7 -> Pawn.Black,
    a2 -> Pawn.White, b2 ->   Pawn.White, c2 ->   Pawn.White, d2 ->  Pawn.White, e2 -> Pawn.White, f2 ->   Pawn.White, g2 ->   Pawn.White, h2 -> Pawn.White,
    a1 -> Rook.White, b1 -> Knight.White, c1 -> Bishop.White, d1 -> Queen.White, e1 -> King.White, f1 -> Bishop.White, g1 -> Knight.White, h1 -> Rook.White)
}

object Game {
  var board: Board = Board(Nil, InitialPieces)
  
  def newGame() = {
    this.board = Board(Nil, InitialPieces)
    Renderer.render(board)
  }
  
  def makeMove(from: Position, to: Position) = {
    board = board.makeMove(Move(from, to))
    Renderer.render(board)
  }
  
  abstract class Mover(from: Position) {
    def a1() = makeMove(from, Position.a1)
    def a2() = makeMove(from, Position.a2)
    def a3() = makeMove(from, Position.a3)
    def a4() = makeMove(from, Position.a4)
    def a5() = makeMove(from, Position.a5)
    def a6() = makeMove(from, Position.a6)
    def a7() = makeMove(from, Position.a7)
    def a8() = makeMove(from, Position.a8)
    def b1() = makeMove(from, Position.b1)
    def b2() = makeMove(from, Position.b2)
    def b3() = makeMove(from, Position.b3)
    def b4() = makeMove(from, Position.b4)
    def b5() = makeMove(from, Position.b5)
    def b6() = makeMove(from, Position.b6)
    def b7() = makeMove(from, Position.b7)
    def b8() = makeMove(from, Position.b8)
    def c1() = makeMove(from, Position.c1)
    def c2() = makeMove(from, Position.c2)
    def c3() = makeMove(from, Position.c3)
    def c4() = makeMove(from, Position.c4)
    def c5() = makeMove(from, Position.c5)
    def c6() = makeMove(from, Position.c6)
    def c7() = makeMove(from, Position.c7)
    def c8() = makeMove(from, Position.c8)
    def d1() = makeMove(from, Position.d1)
    def d2() = makeMove(from, Position.d2)
    def d3() = makeMove(from, Position.d3)
    def d4() = makeMove(from, Position.d4)
    def d5() = makeMove(from, Position.d5)
    def d6() = makeMove(from, Position.d6)
    def d7() = makeMove(from, Position.d7)
    def d8() = makeMove(from, Position.d8)
    def e1() = makeMove(from, Position.e1)
    def e2() = makeMove(from, Position.e2)
    def e3() = makeMove(from, Position.e3)
    def e4() = makeMove(from, Position.e4)
    def e5() = makeMove(from, Position.e5)
    def e6() = makeMove(from, Position.e6)
    def e7() = makeMove(from, Position.e7)
    def e8() = makeMove(from, Position.e8)
    def f1() = makeMove(from, Position.f1)
    def f2() = makeMove(from, Position.f2)
    def f3() = makeMove(from, Position.f3)
    def f4() = makeMove(from, Position.f4)
    def f5() = makeMove(from, Position.f5)
    def f6() = makeMove(from, Position.f6)
    def f7() = makeMove(from, Position.f7)
    def f8() = makeMove(from, Position.f8)
    def g1() = makeMove(from, Position.g1)
    def g2() = makeMove(from, Position.g2)
    def g3() = makeMove(from, Position.g3)
    def g4() = makeMove(from, Position.g4)
    def g5() = makeMove(from, Position.g5)
    def g6() = makeMove(from, Position.g6)
    def g7() = makeMove(from, Position.g7)
    def g8() = makeMove(from, Position.g8)
    def h1() = makeMove(from, Position.h1)
    def h2() = makeMove(from, Position.h2)
    def h3() = makeMove(from, Position.h3)
    def h4() = makeMove(from, Position.h4)
    def h5() = makeMove(from, Position.h5)
    def h6() = makeMove(from, Position.h6)
    def h7() = makeMove(from, Position.h7)
    def h8() = makeMove(from, Position.h8)
  }
  
  object a1 extends Mover(Position.a1)
  object a2 extends Mover(Position.a2)
  object a3 extends Mover(Position.a3)
  object a4 extends Mover(Position.a4)
  object a5 extends Mover(Position.a5)
  object a6 extends Mover(Position.a6)
  object a7 extends Mover(Position.a7)
  object a8 extends Mover(Position.a8)
  object b1 extends Mover(Position.b1)
  object b2 extends Mover(Position.b2)
  object b3 extends Mover(Position.b3)
  object b4 extends Mover(Position.b4)
  object b5 extends Mover(Position.b5)
  object b6 extends Mover(Position.b6)
  object b7 extends Mover(Position.b7)
  object b8 extends Mover(Position.b8)
  object c1 extends Mover(Position.c1)
  object c2 extends Mover(Position.c2)
  object c3 extends Mover(Position.c3)
  object c4 extends Mover(Position.c4)
  object c5 extends Mover(Position.c5)
  object c6 extends Mover(Position.c6)
  object c7 extends Mover(Position.c7)
  object c8 extends Mover(Position.c8)
  object d1 extends Mover(Position.d1)
  object d2 extends Mover(Position.d2)
  object d3 extends Mover(Position.d3)
  object d4 extends Mover(Position.d4)
  object d5 extends Mover(Position.d5)
  object d6 extends Mover(Position.d6)
  object d7 extends Mover(Position.d7)
  object d8 extends Mover(Position.d8)
  object e1 extends Mover(Position.e1)
  object e2 extends Mover(Position.e2)
  object e3 extends Mover(Position.e3)
  object e4 extends Mover(Position.e4)
  object e5 extends Mover(Position.e5)
  object e6 extends Mover(Position.e6)
  object e7 extends Mover(Position.e7)
  object e8 extends Mover(Position.e8)
  object f1 extends Mover(Position.f1)
  object f2 extends Mover(Position.f2)
  object f3 extends Mover(Position.f3)
  object f4 extends Mover(Position.f4)
  object f5 extends Mover(Position.f5)
  object f6 extends Mover(Position.f6)
  object f7 extends Mover(Position.f7)
  object f8 extends Mover(Position.f8)
  object g1 extends Mover(Position.g1)
  object g2 extends Mover(Position.g2)
  object g3 extends Mover(Position.g3)
  object g4 extends Mover(Position.g4)
  object g5 extends Mover(Position.g5)
  object g6 extends Mover(Position.g6)
  object g7 extends Mover(Position.g7)
  object g8 extends Mover(Position.g8)
  object h1 extends Mover(Position.h1)
  object h2 extends Mover(Position.h2)
  object h3 extends Mover(Position.h3)
  object h4 extends Mover(Position.h4)
  object h5 extends Mover(Position.h5)
  object h6 extends Mover(Position.h6)
  object h7 extends Mover(Position.h7)
  object h8 extends Mover(Position.h8)
}

import Game._
