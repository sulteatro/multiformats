package milletre

import java.nio.file.Path
import java.nio.file.Paths
import scala.quoted.*

object macros:
  def currentFilePath(using Quotes): Expr[Path] =
    import quotes.reflect.*

    val strPathExpr: Expr[String] = Expr(Position.ofMacroExpansion.sourceFile.path)
    '{ Paths.get($strPathExpr) }
