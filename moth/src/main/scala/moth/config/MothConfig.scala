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
                             lampLightInitialSignal: Signal,
                             lampDarkInitialSignal: Signal,
                             lampsNumber: Int,
                             initialMothNumber: Int,
                             chance: Double,
                             lampChance: Double,
                             mothChance: Double,
                             mothDeathChance: Double,
                             maleMothChance: Double,
                             mothLampApproachChance: Double,
                             lightLampChance: Double
                           ) extends XinukConfig