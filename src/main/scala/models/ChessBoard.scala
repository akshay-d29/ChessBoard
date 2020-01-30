package models

case class ChessBoard(cells: List[Cell])

object ChessBoard {

  import Cell._

  // todo need to refactor this, as this will always create chessboard of 8x8 grid cells
  def isWithInChessBoard(cell: Cell)(implicit chessBoard: ChessBoard): Either[Error, Cell] = {
    val cells: List[Cell] = chessBoard.cells
    Either.cond(cells.contains(cell), cells.find(_ == cell).get, InvalidPosition)
  }

  def nextPossibleMoves(chessPiece: ChessPiece, cell: Cell): List[String] = {
    chessPiece.canMoveIn.map {
      case Horizontal => horizontalMoves(chessPiece.canTakeStep, cell).map(_.toString).mkString(", ")
      case Vertical => verticalMoves(chessPiece.canTakeStep, cell).map(_.toString).mkString(", ")
      case Diagonal => diagonalMoves(chessPiece.canTakeStep, cell).map(_.toString).mkString(", ")
    }
  }

  private def horizontalMoves(step: Step, cell: Cell): List[Cell] = {
    step match {
      case One => {
        List(rightCell(cell), leftCell(cell))
          .map(isWithInChessBoard)
          .collect { case Right(value) => value }
      }
      case Multiple => allLeftCells(cell) ::: allRightCells(cell)
      case TwoAndHalf => allHorizontalCellsForTwoAndHalfStep(cell)
    }
  }

  private def verticalMoves(step: Step, cell: Cell): List[Cell] = {
    step match {
      case One => {
        List(upwardCell(cell), downwardCell(cell))
          .map(isWithInChessBoard)
          .collect { case Right(value) => value }
      }
      case Multiple => allUpwardCells(cell) ::: allDownwardCells(cell)
      case TwoAndHalf => allVerticalCellsForTwoAndHalfStep(cell)
    }
  }

  private def diagonalMoves(step: Step, cell: Cell): List[Cell] = {
    step match {
      case One => {
        List(diagonalLeftUpward(cell), diagonalLeftDownward(cell), diagonalRightUpward(cell), diagonalRightDownward(cell))
          .map(isWithInChessBoard)
          .collect { case Right(value) => value }
      }
      case Multiple => allDiagonalLeftDownwardCells(cell) ::: allDiagonalLeftUpwardCells(cell) ::: allDiagonalRightDownwardCells(cell) ::: allDiagonalRightUpwardCells(cell)
      case TwoAndHalf => allHorizontalCellsForTwoAndHalfStep(cell) ::: allVerticalCellsForTwoAndHalfStep(cell)
    }
  }

}
