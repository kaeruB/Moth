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

      case (LampCell(currentSmell), EmptyCell(incomingSmell)) =>
        (LampCell(currentSmell + incomingSmell), MothMetrics.empty())

      case (EmptyCell(currentSmell), LampCell(incomingSmell)) =>
        (LampCell(currentSmell + incomingSmell), MothMetrics.empty())

      case (LampCell(currentSmell), LampCell(incomingSmell)) =>
        (LampCell(currentSmell + incomingSmell), MothMetrics.empty())

      case (MothCell(currentSmell, mothType), LampCell(incomingSmell)) =>
        (LampCell(incomingSmell), MothMetrics.empty())

      case (LampCell(currentSmell), MothCell(incomingSmell, mothType)) =>
        (LampCell(currentSmell), MothMetrics.empty())

      case (Obstacle, _) => (Obstacle, MothMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
