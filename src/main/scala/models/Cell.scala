package models

import models.ChessBoard.isWithInChessBoard

import scala.annotation.tailrec

case class Cell(row: Char, num: Int) {
  override def toString: String = s"$row$num"
}

object Cell {
  def prepareCells(n: Int = 8): List[Cell] = {
    val rows = (0 until n).map(n => (n + 'A').toChar).toList
    val columns = (1 to n).toList
    val cells = for {
      r <- rows
      c <- columns
    } yield (r, c)

    cells.map(tuple => Cell(tuple._1, tuple._2))
  }

  def upwardCell(cell: Cell): Cell = cell.copy(cell.row, cell.num + 1)

  def downwardCell(cell: Cell): Cell = cell.copy(cell.row, cell.num - 1)

  def leftCell(cell: Cell): Cell = cell.copy((cell.row - 1).toChar, cell.num)

  def rightCell(cell: Cell): Cell = cell.copy((cell.row + 1).toChar, cell.num)

  def diagonalRightUpward(cell: Cell): Cell = upwardCell(rightCell(cell))

  def diagonalRightDownward(cell: Cell): Cell = downwardCell(rightCell(cell))

  def diagonalLeftUpward(cell: Cell): Cell = upwardCell(leftCell(cell))

  def diagonalLeftDownward(cell: Cell): Cell = downwardCell(leftCell(cell))

  def allHorizontalCellsForTwoAndHalfStep(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val twoStepLeft = leftCell(leftCell(cell))
    val twoStepRight = rightCell(rightCell(cell))
    List(
      upwardCell(twoStepLeft), downwardCell(twoStepLeft),
      upwardCell(twoStepRight), downwardCell(twoStepRight)
    )
  }

  def allVerticalCellsForTwoAndHalfStep(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val twoStepUp = upwardCell(upwardCell(cell))
    val twoStepDown = downwardCell(downwardCell(cell))
    List(
      leftCell(twoStepUp), rightCell(twoStepUp),
      leftCell(twoStepDown), rightCell(twoStepDown),
    )
  }

  @tailrec
  def allRightCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val right = rightCell(cell)
    if (isWithInChessBoard(right).isRight) allRightCells(right, right :: res) else res
  }

  @tailrec
  def allLeftCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val left = leftCell(cell)
    if (isWithInChessBoard(left).isRight) allLeftCells(left, left :: res) else res
  }

  @tailrec
  def allUpwardCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val upward = upwardCell(cell)
    if (isWithInChessBoard(upward).isRight) allUpwardCells(upward, upward :: res) else res
  }

  @tailrec
  def allDownwardCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val downward = downwardCell(cell)
    if (isWithInChessBoard(downward).isRight) allDownwardCells(downward, downward :: res) else res
  }

  @tailrec
  def allDiagonalRightUpwardCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val right = diagonalRightUpward(cell)
    if (isWithInChessBoard(right).isRight) allDiagonalRightUpwardCells(right, right :: res) else
  }

  @tailrec
  def allDiagonalRightDownwardCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val right = diagonalRightDownward(cell)
    if (isWithInChessBoard(right).isRight) allDiagonalRightDownwardCells(right, right :: res) else res
  }

  @tailrec
  def allDiagonalLeftDownwardCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val left = diagonalLeftDownward(cell)
    if (isWithInChessBoard(left).isRight) allDiagonalLeftDownwardCells(left, left :: res) else res
  }

  @tailrec
  def allDiagonalLeftUpwardCells(cell: Cell, res: List[Cell] = Nil): List[Cell] = {
    val left = diagonalLeftUpward(cell)
    if (isWithInChessBoard(left).isRight) allDiagonalLeftUpwardCells(left, left :: res) else res
  }

}
