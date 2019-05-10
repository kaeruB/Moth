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
            case MothCell(_, MothType.Female) => Color.BLUE // Color.PINK
            case MothCell(_, MothType.Male) => Color.BLUE
            case LampCell(_) => Color.YELLOW
            case cell: SmellingCell => Color.BLACK
            //case cell: SmellingCell => cellToColorRegionsPrimitive(cell)
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

  private def cellToColorRegions(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    if (smellValue < 0.00001) {
      val hue = 1f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.001) {
      val hue = 0.65f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else if (smellValue < 0.1) {
      val hue = 0.28f
      val saturation = 1f
      Color.getHSBColor(hue, saturation, brightness)
    } else {
      val hue = 0.11f
      val saturation = 0.69f
      Color.getHSBColor(hue, saturation, brightness)
    }
  }
}

