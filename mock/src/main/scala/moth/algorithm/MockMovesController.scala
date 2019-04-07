package pl.edu.agh.moth.algorithm

import pl.edu.agh.moth.config.MockConfig
import pl.edu.agh.moth.model._
import pl.edu.agh.moth.simulation.MockMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MockMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MockConfig) extends MovesController {

  private val random = new Random(System.nanoTime())

  /*
  metoda tworząca początkowy stan siatki, na której dany węzeł będzie
wykonywać operacje. W tym przypadku jest to domyślna pusta siatka z jedną komórką typu
MockCell na środku. unchan
   */

  override def initialGrid: (Grid, MockMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = MockCell.create(config.mockInitialSignal)

    val metrics = MockMetrics.empty()
    (grid, metrics)
  }

  /*
  metoda wykonująca pojedynczy krok symulacji dla wszystkich elementów siatki.
   */

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MockMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      val destination = (x + random.nextInt(3) - 1, y + random.nextInt(3) - 1)
      val vacatedCell = EmptyCell(cell.smell)
      val occupiedCell = MockCell.create(config.mockInitialSignal)

      newGrid.cells(destination._1)(destination._2) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = occupiedCell
        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    /*
     znajdowane są miejsca położenia komórek typu MockCell, wszystkie pozostałe są kopiowane do nowej siatki
     */

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MockCell(_)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })

    /*
    każda z MockCell przemieszcza się w losowym kierunku (bądź zostaje w miejscu)
     MockCell nie przemieszcza się, jeżeli docelowa komórka jest przeszkodą bądź inną komórką typu MockCell
     */
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MockMetrics.empty())
  }
}