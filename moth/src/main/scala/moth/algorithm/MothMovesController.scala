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

import scala.Option
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
      .sorted(implicitly[Ordering[(Signal, Int)]].reverse)
      .iterator
      .map { case (_, idx) =>
        val (i, j) = neighbourCellCoordinates(idx)
        (i, j, grid.cells(i)(j))
      }
  }

  def selectDestinationCell(possibleDestinations: Iterator[(Int, Int, GridPart)], newGrid: Grid): commons.Opt[(Int, Int, GridPart)] = {
    possibleDestinations
      .map { case (i, j, current) => (i, j, current, newGrid.cells(i)(j)) }
      .collectFirstOpt {
              case (i, j, currentCell@EmptyCell(_), EmptyCell(_)) =>
                (i, j, currentCell)
              case (i, j, currentCell@BufferCell(EmptyCell(_)), BufferCell(EmptyCell(_))) =>
                (i, j, currentCell)
            }
  }

  def searchForFemale(x: Int, y: Int, cell: MothCell, grid: Grid): Option[(Int, Int)] = {
    val neighbours = Grid.neighbourCellCoordinates(x, y)
//    Grid.neighbourCellCoordinates(x, y).foreach(
//      el => {
//        val (femaleCells, otherCells) = (for {
//            x <- neighbours.indices
//          } yield (x, grid.cells(el._1)(el._2))).partition({
//          case (_, MothCell(_, MothType.Female)) =>
//            println(el._1 + " " + el._2)
//            true
//          case ( _, _) => false
//        })
//        femaleCells
//      }
//    )

//    val (femaleCells, otherCells) = (for {
//        x <- neighbours.indices
//      } yield (x, grid.cells(neighbours(x)._1)(neighbours(x)._2))).partition({
//      case (_, MothCell(_, MothType.Female)) =>
//        true
//      case ( _, _) => false
//    })
//

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

//    grid.cells(el._1, el._2) = MothCell.create(config.mothInitialSignal, MothType.Child)

  }

  override def makeMoves(iteration: Long, grid: Grid): (Grid, MothMetrics) = {
    this.grid = grid
    val newGrid = Grid.empty(bufferZone)

    def copyCells(x: Int, y: Int, cell: GridPart): Unit = {
      newGrid.cells(x)(y) = cell
    }

    def copyLampCells(x: Int, y: Int, cell: LampCell): Unit = {
      newGrid.cells(x)(y) = LampCell.create(config.lampInitialSignal)
    }

    def moveMothCells(x: Int, y: Int, cell: MothCell): Unit = {

      // jak jest male to szuka female
      if (cell.mothType == MothType.Male) {
        searchForFemale(x, y, cell, grid) match {
            // jak znajdzie female to na miejsce female pojawia sie child
          case Some((femaleX, femalseY)) => newGrid.cells(femaleX)(femalseY) = MothCell.create(config.mothInitialSignal, MothType.Child)
          case None =>
        }
      }

      val destinations = calculatePossibleDestinations(cell, x, y, grid)
      val destination = selectDestinationCell(destinations, newGrid)

      val mothType: MothType = cell match {
        case MothCell(_, MothType.Female) => MothType.Female
        case MothCell(_, MothType.Male) => MothType.Male
        case _ => null
      }

      // zeby cmy sobie krazyly, a nie lecialy na wprost + wylapywaly lepiej linie smelli
      var destinationX = (destination.get._1 - 1) + random.nextInt(2 + 1)
      if (destinationX >= config.gridSize) destinationX = config.gridSize - 2
      if (destinationX < 0) destinationX = 1

      var destinationY = (destination.get._2 - 1) + random.nextInt( 2 + 1)
      if (destinationY >= config.gridSize) destinationY = config.gridSize - 2
      if (destinationY < 0) destinationY = 1

      val targetCell: GridPart = grid.cells(destinationX)(destinationY)
      // TUTAJ KLUCZ DO SUKCESU Z MOTHAMI LECACYMI DO LAMP:)
      // do nowej celki nie kopiuje zapachu cmy, tylko ten sam zapach co tam byl (moze cmy, moze lampy, moze niczego)
      val occupiedCell = MothCell(targetCell.smell, mothType)
      val vacatedCell = EmptyCell(cell.smell)

      Opt(destinationX, destinationY, targetCell) match {
        case Opt((i, j, EmptyCell(_))) =>
          newGrid.cells(i)(j) = occupiedCell
          newGrid.cells(x)(y) = vacatedCell // podmianka oproznionej celki na vacatedCell
          grid.cells(x)(y) = vacatedCell
        case Opt((i, j, BufferCell(EmptyCell(_)))) =>
          newGrid.cells(i)(j) = occupiedCell
          newGrid.cells(x)(y) = vacatedCell // podmianka oproznionej celki na vacatedCell
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
      case (_, _, MothCell(_, MothType.Child)) => true
      case (_, _, LampCell(_)) => true
      case (_, _, _) => false   //pozostale, w tym lampCell nie przemieszcza sie
    })

    staticCells.foreach({
      case (x, y, cell) => copyCells(x, y, cell)
    })
    dynamicCells.foreach({
      case (x, y, cell: MothCell) => moveMothCells(x, y, cell)
        // zeby smell sie nie tracil - cos to pomoglo, ale w sumie moze nie jest potrzebne
      case (x, y, lamp: LampCell) => copyLampCells(x, y, lamp)
      case (_, _, _) => println("Blad")
    })

    (newGrid, MothMetrics.empty())
  }
}