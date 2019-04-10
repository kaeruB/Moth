package moth.model

import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

// dorobic rozroznianie na meskiego i zenskiego, moze byc boolean albo enum
final case class MothCell(smell: SmellArray) extends SmellingCell{
  override type Self = MothCell

  override def withSmell(smell: SmellArray): MothCell = copy(smell = smell)
}

object MothCell {
  def create(initialSignal: Signal): MothCell = MothCell(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}