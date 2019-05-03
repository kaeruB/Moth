package moth.algorithm

import moth.config.MothConfig
import moth.model._
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet

// pomyslec nad smellem - jak to dziala itp.
final class MothMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MothConfig) extends MovesController{

  private val random = new scala.util.Random(System.nanoTime())
//  private var mothType = random.nextInt(2)
  private var created: Int=  1

//  private var createdMoth =  MothCell.create(config.mothInitialSignal, MothType.Female)

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
          random.nextInt(3) match {
            case 0 =>
              if (random.nextDouble() < config.lampChance){
                LampCell.create(config.lampInitialSignal)
              }
              else{
                grid.cells(x)(y)
              }
            case 1 =>
              if (random.nextDouble() < config.mothChance){
                created = 1
                MothCellFemale.create(config.mothInitialSignal)
              }
              else{
                grid.cells(x)(y)

              }
            case 2 =>
              if (random.nextDouble() < config.mothChance){
                created = 2
                MothCellMale.create(config.mothInitialSignal)
              }
              else{
                grid.cells(x)(y)
              }
          }
      }
    }

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


      var occupiedCell: MothCell = MothCell.create(config.mothInitialSignal, MothType.Male)
      if(created == 1){
        occupiedCell = MothCell.create(config.mothInitialSignal, MothType.Female)

      }

      val vacatedCell = EmptyCell(cell.smell)

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

//      case (_, _, MothCellMale(_) | MothCellFemale(_)) => true //mothCellMale sie przemieszcza
//      case (_, _, MothCellFemale(_)) => true //mothCellFemale sie przemieszcza
      case (_, _, MothCell(_, _)) => true //mothCell sie przemieszcza
      case (_, _, _) => false   //pozostale, w tym lampCell nie przemieszcza sie
    })

    staticCells.foreach({ case (x, y, cell) => copyCells(x, y, cell) })
    dynamicCells.foreach({ case (x, y, cell) => moveCells(x, y, cell) })

    (newGrid, MothMetrics.empty())
  }
}