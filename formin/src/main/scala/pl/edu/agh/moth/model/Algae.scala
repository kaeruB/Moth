package pl.edu.agh.moth.model

import pl.edu.agh.moth.config.ForminConfig
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{BufferCell, EmptyCell, GridPart, SmellingCell}

final case class AlgaeCell(smell: SmellArray, lifespan: Long) extends SmellingCell {
  override type Self = AlgaeCell

  override def withSmell(smell: SmellArray): AlgaeCell = copy(smell = smell)
}

trait AlgaeAccessible[+T <: GridPart] {
  def withAlgae(lifespan: Long): T
}


// kapusta
object AlgaeAccessible {

  // uzyte w inicjalizacji gridu
  def unapply(arg: EmptyCell)(implicit config: ForminConfig): AlgaeAccessible[AlgaeCell] =
    new AlgaeAccessible[AlgaeCell] {
      override def withAlgae(lifespan: Long): AlgaeCell = AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan)
    }

  // to chyba nigdy sie nie stanie, nie znalazlam takiego przypadku w kodzie
  def unapply(arg: BufferCell)(implicit config: ForminConfig): AlgaeAccessible[BufferCell] =
    new AlgaeAccessible[BufferCell] {
      override def withAlgae(lifespan: Long): BufferCell = BufferCell(AlgaeCell(arg.smellWith(config.algaeInitialSignal), lifespan))
    }

  def unapply(arg: GridPart)(implicit config: ForminConfig): Option[AlgaeAccessible[GridPart]] = arg match {
    case cell: EmptyCell => Some(unapply(cell)) // to bierze pierwszy unapply
    case cell: BufferCell => Some(unapply(cell)) //bierze drugi unapply
    case _ => None
  }
}
