package moth.algorithm

import com.avsystem.commons
import com.avsystem.commons.misc.Opt
import com.avsystem.commons.SharedExtensions._
import moth.config.MothConfig
import moth.model.LampType.LampType
import moth.model.MothType.MothType
import moth.model.{LampCell, LampType, MothCell, MothType}
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.algorithm.MovesController
import pl.edu.agh.xinuk.model._

import scala.collection.immutable.TreeSet

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
                if(random.nextDouble()< config.lightLampChance)
                  LampCell.create(config.lampDarkInitialSignal, LampType.Dark)
                else
                  LampCell.create(config.lampLightInitialSignal, LampType.Light)
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

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MothMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone)

    val (dynamicCells, staticCells) = (for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } yield (x, y, grid.cells(x)(y))).partition({
      case (_, _, MothCell(_, MothType.Female)) => true
      case (_, _, MothCell(_, MothType.Male)) => true
      case (_, _, LampCell(_, _)) => true
      case (_, _, _) => false
    })

    staticCells.foreach({
      case (x, y, cell) => copyCells(x, y, cell, newGrid)
    })
    dynamicCells.foreach({
      case (x, y, cell: MothCell) => moveMothCells(x, y, cell, newGrid)
      case (x, y, lamp: LampCell) => copyLampCells(x, y, lamp, newGrid)
      case (_, _, _) => println("Blad")
    })

    (newGrid, MothMetrics.empty())
  }

  def moveMothCells(x: Int, y: Int, cell: MothCell, newGrid: Grid): Unit = {
    multiplicateMoths(x, y, cell, newGrid)

    val vacatedCell = EmptyCell(cell.smell)

    val destinations = calculatePossibleDestinations(cell, x, y, grid)
    val destination = selectDestinationCell(destinations, newGrid)

    if (destination.isEmpty) {
      // nowhere to go, let it burn
      newGrid.cells(x)(y) = vacatedCell
    }
    else {
      val mothType: MothType = cell match {
        case MothCell(_, MothType.Female) => MothType.Female
        case MothCell(_, MothType.Male) => MothType.Male
        case _ => null
      }

      var (destinationX, destinationY, isMothToBurn) = makeMovesMothLike(destination.get._1, destination.get._2, newGrid).get

      val targetCell: GridPart = grid.cells(destinationX)(destinationY)

      val occupiedCell = MothCell(targetCell.smell, mothType)

      Opt(destinationX, destinationY, targetCell) match {
        case Opt((i, j, EmptyCell(_))) =>
          if (!isMothToBurn) newGrid.cells(i)(j) = occupiedCell
          else newGrid.cells(i)(j) = EmptyCell(cell.smell)
          newGrid.cells(x)(y) = vacatedCell
          grid.cells(x)(y) = vacatedCell
        case Opt((i, j, BufferCell(EmptyCell(_)))) =>
          if (!isMothToBurn) newGrid.cells(i)(j) = BufferCell(occupiedCell)
          else newGrid.cells(i)(j) = BufferCell(EmptyCell(cell.smell))
          newGrid.cells(x)(y) = vacatedCell
          grid.cells(x)(y) = vacatedCell
        case _ =>
          newGrid.cells(x)(y) = occupiedCell
      }
    }
  }

  def multiplicateMoths(x: Int, y: Int, cell: MothCell, newGrid: Grid): Unit = {
    if (cell.mothType == MothType.Male) {
      searchForFemale(x, y, cell, grid) match {
        case Some((femaleX, femalseY)) =>
          findEmptyCellForChild(femaleX, femalseY) match {
            case Some((childX, childY)) =>
              if (config.mothDeathChance < config.maleMothChance)
                newGrid.cells(childX)(childY) = MothCell.create(config.mothInitialSignal, MothType.Male)
              else
                newGrid.cells(childX)(childY) = MothCell.create(config.mothInitialSignal, MothType.Female)
            case None =>
          }
        case None =>
      }
    }
  }

  def searchForFemale(x: Int, y: Int, cell: MothCell, grid: Grid): Option[(Int, Int)] = {
    val neighbours = Grid.neighbourCellCoordinates(x, y)
    val (femaleCells, otherCells) = (for {
      i <- neighbours.indices
    } yield (i, grid.cells(neighbours(i)._1)(neighbours(i)._2))).partition({
      case (_, MothCell(_, MothType.Female)) =>
        true
      case ( _, _) => false
    })

    if (femaleCells.nonEmpty)
      Some(neighbours(femaleCells.head._1))
    else
      None
  }

  def findEmptyCellForChild(x: Int, y: Int): Option[(Int, Int)] = {
    val neighbours = Grid.neighbourCellCoordinates(x, y)
    val emptyCells = (for {
      i <- neighbours.indices
    } yield (i, grid.cells(neighbours(i)._1)(neighbours(i)._2), grid.cells(neighbours(i)._1)(neighbours(i)._2).smell))
      .filter {
        el => {
          if (el._2.isInstanceOf[EmptyCell] && el._3.map(_.map(_.value).max).max.toFloat < config.maximalSignalNotBurningMoths)
            true
          else
            false
        }
    }

    if (emptyCells.nonEmpty)
      Some(neighbours(emptyCells.head._1))
    else
      None
  }


  def calculatePossibleDestinations(cell: MothCell, x: Int, y: Int, grid: Grid): Iterator[(Int, Int, GridPart)] = {
    val neighbourCellCoordinates = Grid.neighbourCellCoordinates(x, y)
    Grid.SubcellCoordinates
      .map { case (i, j) => cell.smell(i)(j) }
      .zipWithIndex
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j), current.smell) }
      .filter {
        smell => {
          if (smell._5.map(_.map(_.value).max).max.toFloat < config.maximalSignalNotBurningMoths) {
            true
          }
          else
            false
        }
      }
      .collectFirstOpt {
        case (i, j, currentCell@EmptyCell(_), EmptyCell(_), _) =>
          (i, j, currentCell)
        case (i, j, currentCell@BufferCell(EmptyCell(_)), BufferCell(EmptyCell(_)), _) =>
          (i, j, currentCell)
      }
  }


  def findSafeMove(x: Int, y: Int, newGrid: Grid) : Option[(Int, Int)] = {
    val neighbours = Grid.neighbourCellCoordinates(x, y)
    val safeCellsNotFormated = (for {
      i <- neighbours.indices
    } yield (i, grid.cells(neighbours(i)._1)(neighbours(i)._2).smell))
      .filter{
        el => {
          if (el._2.map(_.map(_.value).max).max.toFloat < config.maximalSignalNotBurningMoths)
            true
          else
            false
        }
      }

    val safeCells = for {
      i <- safeCellsNotFormated.indices
    } yield (neighbours(i)._1, neighbours(i)._2)

    if (safeCells.nonEmpty)
      Some(safeCells.head._1, safeCells.head._2)
    else
      None
  }

  def makeMovesMothLike(x: Int, y: Int, newGrid: Grid) : Option[(Int, Int, Boolean)] = {
    var destinationX = (x - 1) + random.nextInt(2 + 1)
    if (destinationX >= config.gridSize) destinationX = config.gridSize - 2
    if (destinationX < 0) destinationX = 1

    var destinationY = (y - 1) + random.nextInt(2 + 1)
    if (destinationY >= config.gridSize) destinationY = config.gridSize - 2
    if (destinationY < 0) destinationY = 1


    var isMothToBurn = random.nextDouble() > config.mothDeathChance
    if (
      (
        newGrid.cells(destinationX)(destinationY).smell.map(_.map(_.value).max).max.toFloat > config.maximalSignalNotBurningMoths
        || newGrid.cells(destinationX)(destinationY).isInstanceOf[MothCell]
        || newGrid.cells(destinationX)(destinationY).isInstanceOf[LampCell]
        )
      && !isMothToBurn
    ) {
      destinationX = x
      destinationY = y
    }
    else isMothToBurn = false

    Option(destinationX, destinationY, isMothToBurn)
  }

  def copyCells(x: Int, y: Int, cell: GridPart, newGrid: Grid): Unit = {
    newGrid.cells(x)(y) = cell
  }

  def copyLampCells(x: Int, y: Int, cell: LampCell, newGrid: Grid): Unit = {
    val lampType: LampType = cell match {
      case LampCell(_, LampType.Dark) => LampType.Dark
      case LampCell(_, LampType.Light) => LampType.Light
      case _ => null
    }
    if (lampType == LampType.Dark)
      newGrid.cells(x)(y) = LampCell.create(config.lampDarkInitialSignal, lampType)
    else
      newGrid.cells(x)(y) = LampCell.create(config.lampLightInitialSignal, lampType)
  }
}