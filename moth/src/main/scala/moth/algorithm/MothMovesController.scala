package moth.algorithm

import moth.config.MothConfig
import moth.model.{LampCell, MothCell}
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet
import scala.util.Random

final class MothMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MothConfig) extends MovesController {
//
//  private val random = new Random(System.nanoTime())
//
  override def initialGrid: (Grid, MothMetrics) = {
    val grid = Grid.empty(bufferZone)

    grid.cells(config.gridSize / 4)(config.gridSize / 4) = LampCell.create(config.mothInitialSignal)

    val metrics = MothMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MothMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
//      val destination = (x + random.nextInt(3) - 1, y + random.nextInt(3) - 1)
//      val vacatedCell = EmptyCell(cell.smell)
//      val occupiedCell = MockCell.create(config.mockInitialSignal)
//
//      newGrid.cells(destination._1)(destination._2) match {
//        case EmptyCell(_) =>
//          newGrid.cells(x)(y) = vacatedCell
//          newGrid.cells(destination._1)(destination._2) = occupiedCell
//        case BufferCell(EmptyCell(_)) =>
//          newGrid.cells(x)(y) = vacatedCell
//          newGrid.cells(destination._1)(destination._2) = BufferCell(occupiedCell)
//        case _ =>
//          newGrid.cells(x)(y) = occupiedCell
//      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MothCell(_)) => true //mothCell sie przemieszcza
      case (_, _, _) => false   //pozostale, w tym lampCell nie przemieszcza sie
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MothMetrics.empty())
  }
}