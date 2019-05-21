package moth.model.parallel

import moth.config.MothConfig
import moth.model.{LampCell, MothCell, MothType}
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object MothConflictResolver extends ConflictResolver[MothConfig]{

  import Cell._


  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: MothConfig): (GridPart, MothMetrics) = {
    (current, incoming) match {
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), MothMetrics.empty())

      case (MothCell(currentSmell, mothType), EmptyCell(incomingSmell)) =>
        (MothCell(currentSmell + incomingSmell, mothType), MothMetrics.empty())

      case (EmptyCell(currentSmell), MothCell(incomingSmell, mothType)) =>
        (MothCell(currentSmell + incomingSmell, mothType), MothMetrics.empty())

      case (MothCell(currentSmell, mothType), MothCell(incomingSmell, currentMothType)) =>
        (MothCell(currentSmell + incomingSmell, mothType), MothMetrics.empty())

      case (LampCell(currentSmell, lampType), EmptyCell(incomingSmell)) =>
        (LampCell(currentSmell + incomingSmell, lampType), MothMetrics.empty())

      case (EmptyCell(currentSmell), LampCell(incomingSmell, lampType)) =>
        (LampCell(currentSmell + incomingSmell, lampType), MothMetrics.empty())

      case (LampCell(currentSmell, currentLampType), LampCell(incomingSmell, lampType)) =>
        (LampCell(currentSmell + incomingSmell, currentLampType), MothMetrics.empty())

      case (MothCell(currentSmell, mothType), LampCell(incomingSmell, lampType)) =>
        (LampCell(incomingSmell, lampType), MothMetrics.empty())

      case (LampCell(currentSmell, lampType), MothCell(incomingSmell, mothType)) =>
        (LampCell(currentSmell, lampType), MothMetrics.empty())

      case (Obstacle, _) => (Obstacle, MothMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
