package pl.edu.agh.mock.simulation

import pl.edu.agh.xinuk.simulation.Metrics

/*
TEGO NIE TRZEBA RACZEJ

Klasa definiująca kolekcję metryk zbieranych podczas wykonywania symulacji.

Jej tworzeniem zajmuje się na ogół MovesController i ConflictResolver, jednak w przypadku tej symulacji (i
prawdopodobnie projektu wykonywanego w ramach zajęć) nie są zbierane żadne metryki.
 */

final case class MockMetrics() extends Metrics {
  override def log: String = {
    s""
  }

  override def series: Vector[(String, Double)] = Vector()

  override def +(other: Metrics): MockMetrics = {
    this
  }
}

object MockMetrics {
  private val EMPTY = MockMetrics()

  def empty(): MockMetrics = EMPTY
}