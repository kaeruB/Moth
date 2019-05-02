package moth.simulation

import pl.edu.agh.xinuk.simulation.Metrics

final case class MothMetrics(
//                              mothCount: Long,
//                             lightCount: Long,
//                             mothDeaths: Long
                            ) extends Metrics {
  override def log: String = {
    s""
  }

  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): MothMetrics = {
    this
  }
}

object MothMetrics {
  private val EMPTY = MothMetrics()

  def empty(): MothMetrics = EMPTY
}

