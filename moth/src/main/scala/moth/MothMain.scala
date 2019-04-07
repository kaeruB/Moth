package pl.edu.agh.moth

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import moth.algorithm.MothMovesController
import moth.model.{LampCell, MothCell}
import moth.model.parallel.MothConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

/*
konstruuje symulację z pozostałych komponentów
jest tu komponent tłumaczący komórki na kolory
 */

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
            case MothCell(_) => Color.RED
            case LampCell(_) => Color.YELLOW
            case cell: SmellingCell => cellToColorRegions(cell)
          }
        ).start()
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
//    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
//    val brightness = Math.pow(smellValue, 0.1).toFloat
//
//
//    if (smellValue < 0.00001) {
//      val hue = 1f
//      val saturation = 1f
//      Color.getHSBColor(hue, saturation, brightness)
//    } else if (smellValue < 0.001) {
//      val hue = 0.65f
//      val saturation = 1f
//      Color.getHSBColor(hue, saturation, brightness)
//    } else if (smellValue < 0.1) {
//      val hue = 0.28f
//      val saturation = 1f
//      Color.getHSBColor(hue, saturation, brightness)
//    } else {
//      val hue = 0.11f
//      val saturation = 0.69f
//      Color.getHSBColor(hue, saturation, brightness)
//    }
    Color.BLACK
  }

//  private def cellToColor(cell: SmellingCell): Color = {
//    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
//    val brightness = Math.pow(smellValue, 0.1).toFloat
//    val hue = 1f
//    val saturation = 0.69f
//    Color.getHSBColor(hue, saturation, brightness)
//  }

}

