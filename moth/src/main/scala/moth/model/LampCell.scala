package moth.model

import moth.model.LampType.LampType
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}

final case class LampCell(smell: SmellArray, lampType: LampType) extends SmellingCell{
  override type Self = LampCell
  override def withSmell(smell: SmellArray): LampCell = copy(smell = smell)
}

object LampCell {
  def create(initialSignal: Signal, lampType: LampType): LampCell = LampCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), lampType)
}