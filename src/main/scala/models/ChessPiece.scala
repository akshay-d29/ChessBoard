package models

sealed trait ChessPiece {
  def canMoveIn: List[Direction] = this match {
    case King => List(Horizontal, Vertical, Diagonal)
    case Queen => List(Horizontal, Vertical, Diagonal)
    case Bishop => List(Diagonal)
    case Horse => List(Horizontal, Vertical)
    case Rook => List(Horizontal, Vertical)
    case Pawn => List(Vertical)
  }

  def canTakeStep: Step = this match {
    case King => One
    case Queen => Multiple
    case Bishop => Multiple
    case Horse => TwoAndHalf
    case Rook => Multiple
    case Pawn => One
  }
}

case object King extends ChessPiece

case object Queen extends ChessPiece

case object Bishop extends ChessPiece

case object Horse extends ChessPiece

case object Rook extends ChessPiece

case object Pawn extends ChessPiece

object ChessPiece {
  val chessPieces = List(King, Queen, Bishop, Horse, Rook, Pawn)

  def isOneOf(string: String): Either[Error, ChessPiece] = {
    Either.cond(chessPieces.exists(_.toString.toLowerCase == string), chessPieces.find(_.toString.toLowerCase == string).get, InvalidChessPiece)
  }
}