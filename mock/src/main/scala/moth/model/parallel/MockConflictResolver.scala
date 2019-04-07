package pl.edu.agh.moth.model.parallel

import pl.edu.agh.moth.config.MockConfig
import pl.edu.agh.moth.model.MockCell
import pl.edu.agh.moth.simulation.MockMetrics
import pl.edu.agh.xinuk.model._
import pl.edu.agh.xinuk.model.parallel.ConflictResolver

object MockConflictResolver extends ConflictResolver[MockConfig] {

  import Cell._

  override def resolveConflict(current: GridPart, incoming: SmellingCell)(implicit config: MockConfig): (GridPart, MockMetrics) = {
    (current, incoming) match {
      case (Obstacle, _) => /* Jeżeli w danym miejscu znajdowała się przeszkoda (Obstacle), to pozostaje ona bez zmian. */
        (Obstacle, MockMetrics.empty())

        /*
        jeżeli obie były puste, to wynikiem jest pusta
        komórka z sumarycznym sygnałem.
         */
      case (EmptyCell(currentSmell), EmptyCell(incomingSmell)) =>
        (EmptyCell(currentSmell + incomingSmell), MockMetrics.empty())

      /* Jeżeli komórka obecna lub przesyłana jest pusta, */ /*  to jej sygnał jest sumowany z sygnałem
drugiej komórki */
      case (MockCell(currentSmell), EmptyCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell), MockMetrics.empty())  /* to w rezultacie otrzymujemy MockCell z
sumarycznym sygnałem, */

      /* Jeżeli była ona typu MockCell */
      /* to w rezultacie otrzymujemy MockCell z
sumarycznym sygnałem, */
      case (EmptyCell(currentSmell), MockCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell), MockMetrics.empty())

        /*
        Jeżeli obie komórki to MockCell, to występuje kolizja
         dwie     MockCell zastępowane są jedną o sumarycznym sygnale, w rezultacie zmniejszając liczbę
    komórek MockCell w symulacji.
         */
      case (MockCell(currentSmell), MockCell(incomingSmell)) =>
        (MockCell(currentSmell + incomingSmell), MockMetrics.empty())
      case (x, y) => throw new UnsupportedOperationException(s"Unresolved conflict: $x with $y")
    }
  }
}
