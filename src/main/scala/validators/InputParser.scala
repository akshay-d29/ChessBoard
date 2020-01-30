package validators

import models.{Cell, ChessBoard, ChessPiece, Error, InvalidInput, InvalidPosition}

object InputParser {

  private def nonEmpty(str: String): Either[Error, Array[String]] = {
    val condition: Boolean = str.nonEmpty && str.split(" ").length == 2
    Either.cond(condition, str.split(" "), InvalidInput)
  }

  private def validateTypeAndPosition(arr: Array[String]): Either[Error, (ChessPiece, Cell)] = {
    val value = arr.last.map(x => if (x.isDigit) x.asDigit else x.toInt)
    if (value.size == 2) {
      val cell = Cell(value.head.toChar, value.last)
      for {
        piece <- ChessPiece.isOneOf(arr.head.toLowerCase)
        position <- ChessBoard.isWithInChessBoard(cell)
      } yield (piece, position)
    }
    else Left(InvalidPosition)

  }

  def parseInput(chessPieceWithPosition: String): Either[Error, (ChessPiece, Cell)] = nonEmpty(chessPieceWithPosition).flatMap(validateTypeAndPosition)
}
