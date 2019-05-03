package pl.edu.agh.moth.model

import pl.edu.agh.moth.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model._

final case class ForaminiferaCell(energy: Energy, smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = ForaminiferaCell

  override def withSmell(smell: SmellArray): ForaminiferaCell = copy(smell = smell)
}

trait ForaminiferaAccessible[+T <: GridPart] {
  def withForaminifera(energy: Energy, lifespan: Long): T
}


// zajace
object ForaminiferaAccessible {

  // przypadek gdy zajac zje kapuste - dodaje mu sie energia
  def unapply(arg: AlgaeCell)(implicit config: ForminConfig): ForaminiferaAccessible[ForaminiferaCell] =
    new ForaminiferaAccessible[ForaminiferaCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): ForaminiferaCell = ForaminiferaCell(energy + config.algaeEnergeticCapacity, arg.smellWith(config.foraminiferaInitialSignal), lifespan)
    }

  def unapply(arg: EmptyCell)(implicit config: ForminConfig): ForaminiferaAccessible[ForaminiferaCell] =
    new ForaminiferaAccessible[ForaminiferaCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): ForaminiferaCell = ForaminiferaCell(energy, arg.smellWith(config.foraminiferaInitialSignal), lifespan)
    }

  def unapply(arg: BufferCell)(implicit config: ForminConfig): ForaminiferaAccessible[BufferCell] =
    new ForaminiferaAccessible[BufferCell] {
      override def withForaminifera(energy: Energy, lifespan: Long): BufferCell = BufferCell(ForaminiferaCell(energy, arg.smellWith(config.foraminiferaInitialSignal), lifespan))
    }

  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[ForaminiferaAccessible[GridPart]] = arg match {
    case cell: AlgaeCell => Some(unapply(cell))
    case cell: EmptyCell => Some(unapply(cell))
    case cell: BufferCell => Some(unapply(cell))
    case _ => None
  }
}
