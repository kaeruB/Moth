package pl.edu.agh.moth

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.moth.algorithm.MockMovesController
import pl.edu.agh.moth.model.MockCell
import pl.edu.agh.moth.model.parallel.MockConflictResolver
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, Obstacle, SmellingCell}

/*
konstruuje symulację z pozostałych komponentów
jest tu komponent tłumaczący komórki na kolory
 */

object MockMain extends LazyLogging {
  private val configPrefix = "mock"
  private val metricHeaders = Vector()

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation(
      configPrefix,
      metricHeaders,
      MockConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new MockMovesController(_)(_),
      {
        case MockCell(_) => Color.WHITE
        case Obstacle => Color.BLUE
        case cell: SmellingCell => cellToColorRegions(cell)
      }).start()
  }

  private def cellToColorRegions(cell: SmellingCell): Color = {
    /*  Znajdowany jest najsilniejszy
kierunkowy sygnał i zamieniany na jasność.  */
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat

    /*
    kolejne rzędy wielkości otrzymują inne
odcienie, żeby łatwiejsze było rozróżnianie regionów o podobnej sile sygnału
     */
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

  private def cellToColor(cell: SmellingCell): Color = {
    val smellValue = cell.smell.map(_.map(_.value).max).max.toFloat
    val brightness = Math.pow(smellValue, 0.1).toFloat
    val hue = 1f
    val saturation = 0.69f
    Color.getHSBColor(hue, saturation, brightness)
  }

}

