package moth.model

import moth.model.MothType.MothType
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}


// dorobic rozroznianie na meskiego i zenskiego, moze byc boolean albo enum
//final case class MothCell(smell: SmellArray, mothType: MothType) extends SmellingCell{
case class MothCellFemale(smell: SmellArray) extends SmellingCell{
  override type Self = MothCellFemale
  override def withSmell(smell: SmellArray): MothCellFemale = copy(smell = smell)
}


object MothCellFemale {
  def create(initialSignal: Signal): MothCellFemale = MothCellFemale(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}