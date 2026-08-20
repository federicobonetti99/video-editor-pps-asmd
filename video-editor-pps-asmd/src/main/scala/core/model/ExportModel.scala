package core.model

import java.io.File

case class ExportSettings(
                           outputFile: File,
                           width: Int = 1280,
                           height: Int = 720,
                           fps: Int = 30
                         )

case class ExportCommand(
                          executable: String,
                          arguments: Seq[String]
                        )