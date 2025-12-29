package milletre

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.io.BufferedSource
import scala.io.Source

object utils:
  inline def moduleFile: Path = ${ macros.currentFilePath }

  def readFile(filename: String): BufferedSource =
    if filename.contains("://") then Source.fromURL(filename) else Source.fromFile(filename)

  def writeFile(filename: String, content: String): Unit =
    Files.write(Paths.get(filename), content.getBytes(StandardCharsets.UTF_8))

  extension (bytes: Array[Byte])
    def toUtf8: String = new String(bytes, StandardCharsets.UTF_8)
