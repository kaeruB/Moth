package moth.algorithm

import com.avsystem.commons
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.SharedExtensions._
import moth.config.MothConfig
import moth.model.MothType.MothType
import moth.model.{LampCell, MothCell, MothType}
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet


// pomyslec nad smellem - jak to dziala itp.
final class MothMovesController(bufferZone: TreeSet[(Int, Int)])(implicit config: MothConfig) extends MovesController {

  private val random = new scala.util.Random(System.nanoTime())
  private val randomInt = scala.util.Random
  private var created: Int=  1
  private var grid: Grid = _

  override def initialGrid: (Grid, MothMetrics) = {
    grid = Grid.empty(bufferZone)
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
                MothCell.create(config.mothInitialSignal, MothType.Female)
              }
              else{
                grid.cells(x)(y)

              }
            case 2 =>
              if (random.nextDouble() < config.mothChance){
                created = 2
                MothCell.create(config.mothInitialSignal, MothType.Male)
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


  def calculatePossibleDestinations(cell: MothCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse) //.reverse
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }

 /* dwie wersje - nie wiem czy to coś zmienia*/
      // V1
      .collectFirstOpt {
        case (i, j, currentCell, _) =>
          (i, j, currentCell)
      }
    //V2
//      .collectFirstOpt {
//              case (i, j, currentCell@EmptyCell(_), EmptyCell(_)) =>
//                (i, j, currentCell)
//              case (i, j, currentCell@BufferCell(EmptyCell(_)), BufferCell(EmptyCell(_))) =>
//                (i, j, currentCell)
//            }

    // zeby lataly bardziej losowo jak cmy (tak jak w pierwotnej wersji), warto byloby to dodac jakos:
    //      var destinationX = x + random.nextInt(3) - 1
    //      if (destinationX == config.gridSize) destinationX -= 1
    //      if (destinationX == -1) destinationX += 1
    //
    //      var destinationY = y + random.nextInt(3) - 1
    //      if (destinationY == config.gridSize) destinationY -= 1
    //      if (destinationY == -1) destinationY += 1
  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MothMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def moveMothCells(x: Int, y: Int, cell: MothCell): Unit = {
      /*
      1. Może tak być, że jak tutaj kopiujemy smell ćmy to niszczymy(nadpisujemy smellem ciem) smell lamp
      => smell lamp się już dalej nie propaguje. Jeśli tak by było to będzie to pewnie powód dziwnego zachowania ciem.
      2. Do debugowania warto pokolorować smell, coś w Mocku jest z tym. Wtedy zobaczymy co się z tym dzieje :P
       */

      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)

      val vacatedCell = EmptyCell(cell.smell)

      val mothType: MothType = cell match {
        case MothCell(_, MothType.Female) => MothType.Female
        case MothCell(_, MothType.Male) => MothType.Male
        case _ => null
      }
      val occupiedCell = MothCell.create(config.mothInitialSignal, mothType)

      destination match {
        case Opt((i, j, EmptyCell(_))) =>
          newGrid.cells(i)(j) = occupiedCell
          newGrid.cells(x)(y) = vacatedCell
          grid.cells(x)(y) = vacatedCell
        case Opt((i, j, BufferCell(EmptyCell(_)))) =>
          newGrid.cells(i)(j) = occupiedCell
          newGrid.cells(x)(y) = vacatedCell
          grid.cells(x)(y) = vacatedCell
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MothCell(_, MothType.Female)) => true
      case (_, _, MothCell(_, MothType.Male)) => true
      case (_, _, _) => false   //pozostale, w tym lampCell nie przemieszcza sie
    })

    staticCells.foreach({
      case (x, y, cell) => copyCells(x, y, cell)
    })
    dynamicCells.foreach({
      case (x, y, cell: MothCell) => moveMothCells(x, y, cell)
      case _ =>
    })

    (newGrid, MothMetrics.empty())
  }
}