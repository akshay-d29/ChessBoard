import java.io.{BufferedReader, InputStreamReader}

import models.Cell.prepareCells
import models.{ChessBoard, Error}
import validators.InputParser

object ChessBoardSimulator {
  

  def getPossibleMovesForChessPiece(str: String)(chessBoard: ChessBoard): Either[Error, String] =
    InputParser.parseInput(str).map(tuple => ChessBoard.nextPossibleMoves(tuple._1, tuple._2).mkString(", "))
}

// todo need to take chessboard grid as input eg. 8x8 6x6

object Main extends App {

implicit val createChessBoardOf: ChessBoard = ChessBoard(prepareCells(8))
  val bufferedReader = new BufferedReader(new InputStreamReader(System.in))
  val input = bufferedReader.readLine()

  println(ChessBoardSimulator.getPossibleMovesForChessPiece(input)(createChessBoardOf))
}
