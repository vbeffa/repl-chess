/**
 * Chess game for the scala repl.
 */

object Color extends Enumeration {
  val Black, White = Value
}

case class Square(color: Color.Value, rank: Int, file: Char) {
  import Color._
  
  def draw(line: Int) = {
    color match {
      case Black => print("@@@@@@@")
      case White => print("       ")
    }
  }
}

case class Row(rank: Int) {
  import Color._
  
  val squares = {
    ('a' to 'h') map { file =>
      if ((rank % 2 == 1 && file % 2 == 1) || (rank % 2 == 0 && file % 2 == 0)) Square(Black, rank, file)
      else Square(White, rank, file)
    }
  }.toArray
  
  def draw() = {
    println("----------------------------------------------------------------")
    (1 to 3) foreach { line =>
      ('a' to 'h') foreach { file =>
        print("|")
        squares(file - 'a').draw(line)
        if (file == 'h')
          print("|")
      }
      println()
    }
    if (rank == 1)
      println("----------------------------------------------------------------")
  }
}

object Board {
  val squares = {
    (1 to 8).reverse map Row 
  }.toArray
  
  def draw() = squares foreach { _.draw() }
}
