
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "introtosclafx",
    libraryDependencies ++= {
      // Determine OS version of JavaFX binaries
      val osName = System.getProperty("os.name") match {
        case n if n.startsWith("Linux")   => "linux"
        case n if n.startsWith("Mac")     =>
          val arch = System.getProperty("os.arch")
          if (arch == "aarch64" || arch == "arm64") "mac-aarch64" else "mac"
        case n if n.startsWith("Windows") => "win"
        case _                            => throw new Exception("Unknown platform!")
      }
      Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
        .map(m => "org.openjfx" % s"javafx-$m" % "25" classifier osName)
    },
    libraryDependencies ++= Seq(
      "org.scalafx" %% "scalafx" % "24.0.2-R36",
      "com.github.tototoshi" %% "scala-csv" % "2.0.0"
  )
//enable for sbt-assembly
//assembly / assemblyMergeStrategy := {
//  case PathList("META-INF", xs @ _*) => MergeStrategy.discard // Discard all META-INF files
//  case PathList("reference.conf")    => MergeStrategy.concat  // Concatenate config files
//  case PathList(ps @ _*) if ps.last.endsWith(".class") => MergeStrategy.first // Take the first class file
//  case _ => MergeStrategy.first // Apply first strategy to any other file
//}
