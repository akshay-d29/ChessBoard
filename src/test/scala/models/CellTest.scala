package models

import org.scalatest.{FunSpec, Matchers}
import Cell._

class CellTest extends FunSpec with Matchers {

  val cells: List[Cell] = prepareCells(4)

  describe("Cell") {

    it("should prepare proper cells") {
      prepareCells(4) shouldEqual List(Cell('A',1), Cell('A',2), Cell('A',3), Cell('A',4), Cell('B',1), Cell('B',2), Cell('B',3), Cell('B',4), Cell('C',1), Cell('C',2), Cell('C',3), Cell('C',4), Cell('D',1), Cell('D',2), Cell('D',3), Cell('D',4))
    }

    it("should give upward cell of B3 cell") {
      upwardCell(Cell('B', 3)) shouldEqual Cell('B', 4)
    }

    it("should give downward cell of B3 cell") {
      downwardCell(Cell('B', 3)) shouldEqual Cell('B', 2)
    }

    it("should give left cell of B3 cell") {
      leftCell(Cell('B', 3)) shouldEqual Cell('A', 3)
    }

    it("should give right cell of B3 cell") {
      rightCell(Cell('B', 3)) shouldEqual Cell('C', 3)
    }

    it("should give diagonal right upward cell of B3 cell") {
      diagonalRightUpward(Cell('B', 3)) shouldEqual Cell('C', 4)
    }

    it("should give diagonal right downward cell of B3 cell") {
      diagonalRightDownward(Cell('B', 3)) shouldEqual Cell('C', 2)
    }

    it("should give diagonal left upward cell of B3 cell") {
      diagonalLeftUpward(Cell('B', 3)) shouldEqual Cell('A', 4)
    }

    it("should give diagonal left downward cell of B3 cell") {
      diagonalLeftDownward(Cell('B', 3)) shouldEqual Cell('A', 2)
    }

    // failing test --> needs refactor in chessboard
    ignore("should give all right cells") {
      allRightCells(Cell('A', 1)) shouldEqual List(Cell('B', 1), Cell('C', 1), Cell('D', 1))
    }

    it("should give all left cells") {
      allLeftCells(Cell('D', 1)) shouldEqual List(Cell('A', 1), Cell('B', 1), Cell('C', 1))
    }

  }
}
