package moth

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import moth.algorithm.MothMovesController
import moth.model.{LampCell, MothCell, MothType}
import moth.model.parallel.MothConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

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
          new MothMovesController(_)(_),
          {
            case MothCell(_, MothType.Female) => Color.PINK
            case MothCell(_, MothType.Male) => Color.BLUE
            case LampCell(_) => Color.YELLOW
            case cell: SmellingCell => Color.BLACK
          }
        ).start()
  }
}

