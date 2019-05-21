package moth

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import moth.algorithm.MothMovesController
import moth.config.MothConfig
import moth.model.{LampCell, LampType, MothCell, MothType}
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
            case MothCell(_, MothType.Female) => Color.PINK
            case MothCell(_, MothType.Male) => Color.BLUE
            case MothCell(_, MothType.Child) => Color.RED
            case LampCell(_, LampType.Light) => new Color(255, 255, 102)  //light yellow
            case LampCell(_, LampType.Dark) => new Color(255, 128, 0) //dark yellow- orange
            case cell: SmellingCell => Color.BLACK
              // DO TESTOW - KOLORKI:
//            case cell: SmellingCell => cellToColorRegionsPrimitive(cell)
          }
        ).start()
  }

  private def cellToColorRegionsPrimitive(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    if (smellValue > 0.8) Color.RED
    else if (smellValue > 0.3 && smellValue <= 0.8) Color.PINK
    else if (smellValue > 0 && smellValue <= 0.3) Color.WHITE // baaardzo maly smell, ale czemu przy lampie jest maly smell, a dalej wiekszy?
    else Color.BLACK
  }
}

