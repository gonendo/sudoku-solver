import org.scalatest.FunSuite
import sudoku.Sudoku
import sudoku.Solver

class SudokuTests extends FunSuite{
  test("test"){
    val solvedGrid : String = Solver.solve(Sudoku.createGrid())
    assert(solvedGrid == "693184257218675493547932816456398721931247568782516934164859372829763145375421689")
  }
}
