package moth.model.parallel

import moth.config.MothConfig
import moth.model.{MothCell, MothType}
import moth.simulation.MothMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object MothConflictResolver extends ConflictResolver[MothConfig]{

  import Cell._


  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: MothConfig): (GridPart, MothMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) =>
        (Obstacle, MothMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
