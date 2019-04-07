package moth.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

final case class MothConfig(
                             gridSize: Int,
                             guiCellSize: Int,
                             signalSuppressionFactor: Double,
                             signalAttenuationFactor: Double,
                             workersRoot: Int,
                             shardingMod: Int,

                             guiType: GuiType,
                             isSupervisor: Boolean,
                             signalSpeedRatio: Int,
                             iterationsNumber: Long,

                             mothInitialSignal: Signal,
                             lampInitialSignal: Signal,
                             lampsNumber: Int,
                             initialMothNumber: Int
                           ) extends XinukConfig