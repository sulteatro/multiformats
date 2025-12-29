val scala3Version = "3.7.3"

inThisBuild(
  List(
    scalaVersion := scala3Version,
    semanticdbEnabled := true
  )
)

lazy val generateMulticodec = taskKey[Unit]("Rewrite the Multicodec enums from the source of truth")
lazy val generateMultibase = taskKey[Unit]("Rewrite the Multibase enums from the source of truth")

lazy val root = project
  .in(file("."))
  .aggregate(milletre, multiformats)
  .settings(
    publish / skip := true
  )

lazy val milletre = project
  .in(file("milletre"))
  .settings(
    name := "milletre",
    version := "0.0.1",
    organization := "org.sulteatro",
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Wunused:imports",
      "-source:3.7"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test
    ),
    packageSrc / publishArtifact := true
  )

lazy val multiformats = project
  .in(file("multiformats"))
  .dependsOn(milletre)
  .settings(
    name := "multiformats",
    version := "0.0.1",
    organization := "org.sulteatro",
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-Wunused:imports",
      "-source:3.7"
    ),
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0" % Test,
      "org.bouncycastle" % "bcprov-jdk18on" % "1.80"
    ),
    packageSrc / publishArtifact := true,
    generateMulticodec := (Compile / runMain).toTask(" multiformats.multicodec.generate").value,
    generateMultibase := (Compile / runMain).toTask(" multiformats.multibase.generate").value
  )

// Corrects a bug in tab completion in sbt console - see link to joern-cli
// https://github.com/scala/scala3/issues/20421
// Affects all scala versions above 3.3.3
def removeModuleInfoFromJars(report: UpdateReport): UpdateReport = {
  import java.net.URI
  import java.nio.file.{Files, FileSystems}
  import scala.collection.JavaConverters._

  // remove all `/module-info.class` from all jars
  report.allFiles
    .filter(_.getName.endsWith(".jar"))
    .foreach { jar =>
      val zipFs =
        FileSystems.newFileSystem(URI.create(s"jar:file:${jar}"), Map("create" -> "true").asJava)
      zipFs.getRootDirectories.forEach { zipRootDir =>
        Files.list(zipRootDir).filter(_.toString.endsWith("module-info.class")).forEach {
          moduleInfoClass =>
            Files.delete(moduleInfoClass)
        }
      }
      zipFs.close()
    }

  report
}

update := removeModuleInfoFromJars(update.value)
