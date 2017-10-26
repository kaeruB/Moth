package pl.edu.agh.formin

import org.scalatest.{BeforeAndAfter, FlatSpecLike, Matchers}
import pl.edu.agh.formin.config.{ForminConfig, GuiType}
import pl.edu.agh.formin.model._

class GridTest extends FlatSpecLike with Matchers with BeforeAndAfter {

  implicit val config = ForminConfig(
    foraminiferaStartEnergy = Energy(0.5),
    foraminiferaReproductionCost = Energy(0.2),
    foraminiferaReproductionThreshold = Energy(0.3),
    foraminiferaLifeActivityCost = Energy(0.1),
    algaeReproductionFrequency = 2,
    algaeEnergeticCapacity = Energy(0.4),
    signalSpeedRatio = 2,
    signalSuppressionFactor = 0.5,
    gridSize = 5,
    spawnChance = 0.1,
    foraminiferaSpawnChance = 0.5,
    foraminiferaInitialSignal = Signal(-1),
    algaeInitialSignal = Signal(1),
    guiType = GuiType.None,
    guiCellSize = 4,
    workersRoot = 1,
    iterationsNumber = 1000,
    isSupervisor = true
  )

  private var grid = Grid.empty(Set.empty)

  before {
    grid = Grid.empty(Set.empty)
  }

  "A Grid" should "have obstacles around" in {

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      if (x == 0 || x == config.gridSize - 1 && y == 0 || y == config.gridSize - 1) {
        assert(grid.cells(x)(y).isInstanceOf[Obstacle.type])
      }
    }
  }

  it should "have empty cells inside" in {

    for {
      x <- 0 until config.gridSize
      y <- 0 until config.gridSize
    } {
      if (x != 0 && x != config.gridSize - 1 && y != 0 && y != config.gridSize - 1) {
        assert(grid.cells(x)(y).isInstanceOf[EmptyCell])
      }
    }
  }

  it should "propagate signal correctly for one foraminifera cell" in {
    grid.cells(2)(2) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(3)(2) = grid.propagatedSignal(3, 2)
    grid.cells(3)(1) = grid.propagatedSignal(3, 1)

    grid.cells(2)(2).smell(0)(0).value should be(-1)
    grid.cells(2)(2).smell(0)(1).value should be(-1)
    grid.cells(2)(2).smell(0)(2).value should be(-1)
    grid.cells(3)(2).smell(0)(1).value should be(-1.5)

    grid.cells(2)(2).smell(0)(2).value should be(-1)
    grid.cells(3)(1).smell(0)(2).value should be(-0.5)
  }

  it should "propagate signal correctly for one algae cell" in {
    grid.cells(2)(2) = EmptyCell.Instance.withAlgae(0)
    grid.cells(3)(2) = grid.propagatedSignal(3, 2)
    grid.cells(3)(1) = grid.propagatedSignal(3, 1)

    grid.cells(2)(2).smell(0)(0).value should be(1)
    grid.cells(2)(2).smell(0)(1).value should be(1)
    grid.cells(2)(2).smell(0)(2).value should be(1)
    grid.cells(3)(2).smell(0)(1).value should be(1.5)

    grid.cells(2)(2).smell(0)(2).value should be(1)
    grid.cells(3)(1).smell(0)(2).value should be(0.5)
  }

  it should "propagate signal correctly between algae and foraminifera cells" in {
    grid.cells(2)(2) = EmptyCell.Instance.withAlgae(0)
    grid.cells(3)(2) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    val gridCellWithAlgaeAfterSignalPropagation = grid.propagatedSignal(2, 2)
    val gridCellWithForaminiferaAfterSignalPropagation = grid.propagatedSignal(3, 2)
    grid.cells(2)(2) = gridCellWithAlgaeAfterSignalPropagation
    grid.cells(3)(2) = gridCellWithForaminiferaAfterSignalPropagation

    grid.cells(2)(2).smell(2)(1).value should be(-0.5)
    grid.cells(2)(2).smell(2)(0).value should be(1)
    grid.cells(2)(2).smell(2)(2).value should be(1)
    grid.cells(2)(2).smell(0)(0).value should be(1)
    grid.cells(2)(2).smell(0)(1).value should be(1)
    grid.cells(2)(2).smell(0)(2).value should be(1)

    grid.cells(3)(2).smell(0)(0).value should be(-1)
    grid.cells(3)(2).smell(0)(1).value should be(0.5)
    grid.cells(3)(2).smell(0)(2).value should be(-1)
    grid.cells(3)(2).smell(2)(0).value should be(-1)
    grid.cells(3)(2).smell(2)(1).value should be(-1)
    grid.cells(3)(2).smell(2)(2).value should be(-1)
  }

  it should "not propagate signal on obstacle cell" in {
    grid.cells(3)(2) = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    grid.cells(4)(2) = grid.propagatedSignal(4, 2)
    grid.cells(3)(2).smell(1)(1).value should be(-1)
    grid.cells(4)(2).smell(0)(1).value should be(0)
  }

  it should "calculate neighbour cells correctly for middle one" in {
    val gridNeighbourCellCoordinates = Grid.neighbourCellCoordinates(2, 2)
    gridNeighbourCellCoordinates.size should be(8)
    gridNeighbourCellCoordinates.head should be(1, 1)
    gridNeighbourCellCoordinates(gridNeighbourCellCoordinates.size - 1) should be(3, 3)
  }

  it should "calculate neighbour cells correctly for border one" in {
    val gridNeighbourCellCoordinates = Grid.neighbourCellCoordinates(0, 2)
    gridNeighbourCellCoordinates.size should be(8)
    gridNeighbourCellCoordinates.head should be(-1, 1)
    gridNeighbourCellCoordinates(gridNeighbourCellCoordinates.size - 1) should be(1, 3)
  }

  it should "make correct cell transformations from empty cell" in {
    val emptyCell = EmptyCell.Instance
    val emptyCellWithForaminiferaInstantiated = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    val emptyCellWithAlgaeInstantiated = EmptyCell.Instance.withAlgae(0)
    val emptyCellWithSmell = emptyCell.withSmell(emptyCellWithForaminiferaInstantiated.smell)
    val emptyCellWithForaminifera = emptyCell.withForaminifera(Energy(20), 3)
    val emptyCellWithAlgae = emptyCell.withAlgae(6)

    assert(emptyCellWithSmell.isInstanceOf[EmptyCell])
    emptyCellWithSmell.smell should be(emptyCellWithForaminiferaInstantiated.smell)

    assert(emptyCellWithForaminifera.isInstanceOf[ForaminiferaCell])
    emptyCellWithForaminifera.smell should be(emptyCellWithForaminiferaInstantiated.smell)
    emptyCellWithForaminifera.energy should be(Energy(20))
    emptyCellWithForaminifera.lifespan should be(3)

    assert(emptyCellWithAlgae.isInstanceOf[AlgaeCell])
    emptyCellWithAlgae.smell should be(emptyCellWithAlgaeInstantiated.smell)
    emptyCellWithAlgae.lifespan should be(6)
  }

  it should "make correct cell transformations from foraminifera cell" in {
    val foraminiferaCell = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    val emptyCellWithAlgaeInstantiated = EmptyCell.Instance.withAlgae(0)
    val foraminiferaCellWithSmell = foraminiferaCell.withSmell(emptyCellWithAlgaeInstantiated.smell)

    assert(foraminiferaCellWithSmell.isInstanceOf[ForaminiferaCell])
    foraminiferaCellWithSmell.smell should be(emptyCellWithAlgaeInstantiated.smell)
    foraminiferaCellWithSmell.energy should be(config.foraminiferaStartEnergy)
    foraminiferaCellWithSmell.lifespan should be(0)
  }

  it should "make correct cell transformations from algae cell" in {
    val algaeCell = EmptyCell.Instance.withAlgae(0)
    val emptyCellWithForaminiferaInstantiated = EmptyCell.Instance.withForaminifera(config.foraminiferaStartEnergy, 0)
    val emptyCell = EmptyCell.Instance
    val algaeCellWithSmell = algaeCell.withSmell(emptyCellWithForaminiferaInstantiated.smell)
    val algaeCellWithForaminifera = algaeCell.withForaminifera(Energy(20), 3)

    assert(algaeCellWithSmell.isInstanceOf[AlgaeCell])
    algaeCellWithSmell.smell should be(emptyCellWithForaminiferaInstantiated.smell)
    algaeCellWithSmell.lifespan should be(0)

    assert(algaeCellWithForaminifera.isInstanceOf[ForaminiferaCell])
    algaeCellWithForaminifera.smell should be(emptyCell.smell)
    algaeCellWithForaminifera.energy should be(Energy(20.4))
    algaeCellWithForaminifera.lifespan should be(3)
  }
}
