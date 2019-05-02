package moth.algorithm

import moth.config.MothConfig
import moth.model.MothType.{Female, MothType}
import moth.model.{LampCell, MothCell, MothType}
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet

// pomyslec nad smellem - jak to dziala itp.
final class MothMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MothConfig) extends MovesController{

  private val random = new scala.util.Random(System.nanoTime())
  private val randomInt = scala.util.Random
//  var mothType = 1

  override def initialGrid: (Grid, MothMetrics) = {
    val grid = Grid.empty(bufferZone)

    //do przemyslenia czy config.lampInitialSignal ma sens - powinien byc raczej inny dla lamp i dla moth (jeden dodatni, drugi ujemny - do sprawdzenia)
    // wartosc oczekiwana w ustawianiu na poczatku lamp, zeby jak bedzie wiecej workerow to nie bylo na kazdej planszy
    // po tyle samo lamp tylko losowo (podobnie jak w torch, a nie robic tak jak w mock, ze kazde okienko ma tyle samo lamp)
    // to samo sie tyczy ciem


    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
      if x != 0 && y !=0 && x != config.gridSize - 1 && y != config.gridSize -1
    }{
      if(random.nextDouble() < config.chance){
        grid.cells(x)(y) =
          random.nextInt(2) match {
            case 0 =>
              if (random.nextDouble() < config.lampChance){
                LampCell.create(config.lampInitialSignal)
              }
              else{
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.mothChance){
                MothCell.create(config.mothInitialSignal, MothType.Female)
              }
              else{
                grid.cells(x)(y)
              }
          }
      }
    }


////    ustawienie lamp na gridzie
//    for (i <- 1 to config.lampsNumber)
//      grid.cells(randomInt.nextInt(config.gridSize))(randomInt.nextInt(config.gridSize)) = LampCell.create(config.lampInitialSignal)
//
////    ustawienie ciem na gridzie
//    for (i <- 1 to config.initialMothNumber) {
//      var randX = randomInt.nextInt(config.gridSize)
//      var randY = randomInt.nextInt(config.gridSize)
////      while (w grid.cells(randX)(randY) jest juz LampCell) {
////        randX = randomInt.nextInt(config.gridSize)
////        randY = randomInt.nextInt(config.gridSize)
////      }
//      grid.cells(randX)(randY) = MothCell.create(config.mothInitialSignal)
//    }

    val metrics = MothMetrics.empty()
    (grid, metrics)
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MothMetrics) = {
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    // to do przepatrzenia czy ok dla MothCell
    def moveCells(x: Int, y: Int, cell: GridPart): Unit = {
      var destinationX = x + random.nextInt(3) - 1
      if (destinationX == config.gridSize) destinationX -= 1
      if (destinationX == -1) destinationX += 1

      var destinationY = y + random.nextInt(3) - 1
      if (destinationY == config.gridSize) destinationY -= 1
      if (destinationY == -1) destinationY += 1

      val vacatedCell = EmptyCell(cell.smell)
      val occupiedCell = MothCell.create(config.mothInitialSignal, MothType.Female)

      //do poprawienia
      newGrid.cells(destinationX)(destinationY) match {
        case EmptyCell(_) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destinationX)(destinationY) = occupiedCell
        case BufferCell(EmptyCell(_)) =>
          newGrid.cells(x)(y) = vacatedCell
          newGrid.cells(destinationX)(destinationY) = BufferCell(occupiedCell)
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MothCell(_, _)) => true //mothCell sie przemieszcza
      case (_, _, _) => false   //pozostale, w tym lampCell nie przemieszcza sie
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MothMetrics.empty())
  }
}