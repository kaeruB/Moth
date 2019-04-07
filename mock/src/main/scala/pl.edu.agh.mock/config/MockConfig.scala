package pl.edu.agh.mock.config

import pl.edu.agh.xinuk.config.{GuiType, XinukConfig}
import pl.edu.agh.xinuk.model.Signal

  /*
  Klasa definiująca konfigurację symulacji.

  Większość parametrów tutaj zawartych to wartości
wymagane przez framework do działania (zdefiniowane również w XinukConfig)


Program uruchamiany jest domyślnie z konfiguracją wczytaną z pliku resources/reference.conf
   */


final case class MockConfig(
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

                             mockInitialSignal: Signal /*  sygnał emitowany przez komórkę typu MockCell */
                           ) extends XinukConfig