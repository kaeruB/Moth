package moth.model

import moth.model.MothType.MothType
import pl.edu.agh.xinuk.model.Cell.SmellArray
import pl.edu.agh.xinuk.model.{Cell, Signal, SmellingCell}


// dorobic rozroznianie na meskiego i zenskiego, moze byc boolean albo enum
//final case class MothCell(smell: SmellArray, mothType: MothType) extends SmellingCell{
case class MothCellMale(smell: SmellArray) extends SmellingCell{
  override type Self = MothCellMale
  override def withSmell(smell: SmellArray): MothCellMale = copy(smell = smell)
}


object MothCellMale {
  def create(initialSignal: Signal): MothCellMale = MothCellMale(Array.fill(Cell.Size, Cell.Size)(initialSignal))
}