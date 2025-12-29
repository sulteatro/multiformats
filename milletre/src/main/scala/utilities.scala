package milletre.utilities

import java.nio.charset.StandardCharsets

extension (bytes: Array[Byte])
  def toUtf8: String = new String(bytes, StandardCharsets.UTF_8)
