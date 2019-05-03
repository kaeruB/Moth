package pl.edu.agh.moth

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import moth.algorithm.MothMovesController
import moth.config.MothConfig
import moth.model.MothType.MothType
import moth.model._
import moth.model.parallel.MothConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

import scala.collection.immutable.TreeSet

object MothMain extends LazyLogging {
  private val configPrefix = "moth"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      MothConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(
          new MothMovesController(_:TreeSet[(Int, Int)])(_:MothConfig),
          {
            case MothCellMale(_) => Color.BLUE

            case MothCellFemale(_) => Color.RED

//            case m: MothCellFemale => Color.RED
            case LampCell(_) => Color.YELLOW
            case cell: SmellingCell => Color.BLACK
          }
        ).start()
  }
}

