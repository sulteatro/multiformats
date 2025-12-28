package milletre

import scala.quoted.*
import java.nio.file.{Path, Paths}

object macros:
  def currentFilePath(using Quotes): Expr[Path] =
    import quotes.reflect.*

    val strPathExpr: Expr[String] = Expr(Position.ofMacroExpansion.sourceFile.path)
    '{ Paths.get($strPathExpr) }
