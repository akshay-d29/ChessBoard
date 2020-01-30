import org.scalatest.{FunSpec, Matchers}

class ChessBoardSimulatorTest extends FunSpec with Matchers {

  describe("Chess Board - simulate movement of chess piece on empty chess board") {
    it("should return all possible cells in which the King can move") {
      val input: String = "King D5"
      val expectedOutput = Right("E5, C5, D6, D4, C6, C4, E6, E4")

      ChessBoardSimulator.getPossibleMovesForChessPiece(input) shouldEqual expectedOutput
    }

    it("should return all possible cells in which the Horse can move") {
      val input: String = "Horse E3"
      val expectedOutput = Right("C4, C2, G4, G2, D5, F5, D1, F1")

      ChessBoardSimulator.getPossibleMovesForChessPiece(input) shouldEqual expectedOutput
    }

    it("should return all possible cells in which the Rook can move") {
      val input: String = "Rook A1"
      val expectedOutput = Right("H1, G1, F1, E1, D1, C1, B1, A8, A7, A6, A5, A4, A3, A2")

      ChessBoardSimulator.getPossibleMovesForChessPiece(input) shouldEqual expectedOutput
    }

    it("should return all possible cells in which the Queen can move") {
      val input: String = "Queen D5"
      val expectedOutput = Right("A5, B5, C5, H5, G5, F5, E5, D8, D7, D6, D1, D2, D3, D4, A2, B3, C4, A8, B7, C6, H1, G2, F3, E4, G8, F7, E6")

      ChessBoardSimulator.getPossibleMovesForChessPiece(input) shouldEqual expectedOutput
    }

    it("should return all possible cells in which the Bishop can move") {
      val input: String = "Bishop D5"
      val expectedOutput = Right("A2, B3, C4, A8, B7, C6, H1, G2, F3, E4, G8, F7, E6")

      ChessBoardSimulator.getPossibleMovesForChessPiece(input) shouldEqual expectedOutput
    }
  }

}
