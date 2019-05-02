package moth.model

import moth.model.MothType.MothType
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}


// dorobic rozroznianie na meskiego i zenskiego, moze byc boolean albo enum
//final case class MothCell(smell: SmellArray, mothType: MothType) extends SmellingCell{
final case class MothCell(smell: SmellArray, mothType: MothType) extends SmellingCell{
  override type Self = MothCell
  override def withSmell(smell: SmellArray): MothCell = copy(smell = smell)
}


object MothCell {
  def create(initialSignal: Signal, mothType: MothType): MothCell = MothCell(Array.fill(Cell.Size, Cell.Size)(initialSignal), mothType)
}