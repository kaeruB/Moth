package pl.edu.agh.moth

import java.awt.Color

import com.typesafe.scalalogging.LazyLogging
import pl.edu.agh.moth.algorithm.ForminMovesController
import pl.edu.agh.moth.config.ForminConfig
import pl.edu.agh.moth.model.parallel.ForminConflictResolver
import pl.edu.agh.moth.model.{AlgaeCell, ForaminiferaCell}
import pl.edu.agh.xinuk.Simulation
import pl.edu.agh.xinuk.model.{DefaultSmellPropagation, SmellingCell}

object ForminMain extends LazyLogging {
  private val configPrefix = "formin"
  private val metricHeaders = Vector(
    "foraminiferaCount",
    "algaeCount",
    "foraminiferaDeaths",
    "foraminiferaTotalEnergy",
    "foraminiferaReproductionsCount",
    "consumedAlgaeCount",
    "foraminiferaTotalLifespan",
    "algaeTotalLifespan"
  )

  private def cellToColor(cell: SmellingCell): Color = {
    cell match {
      case AlgaeCell(_, _) => new Color(0, 128, 0) //zielony
      case ForaminiferaCell(_, _, _) => new Color(139, 69, 19) //brazowy
      case _ => Color.WHITE
    }
  }

  def main(args: Array[String]): Unit = {
    import pl.edu.agh.xinuk.config.ValueReaders._
    new Simulation[ForminConfig](configPrefix, metricHeaders, ForminConflictResolver,
      DefaultSmellPropagation.calculateSmellAddendsStandard)(new ForminMovesController(_)(_),
      { case cell: SmellingCell => cellToColor(cell) }
    ).start()
  }

}
