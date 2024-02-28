
name := "Sega-MD"

version := "0.9"

scalaVersion := "3.3.1"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-encoding","ISO-8859-1",
  "-deprecation"
)

// Generates Version.scala
Compile / sourceGenerators += Def.task {
  val file = (Compile / scalaSource).value / "ucesoft" / "smd" / "Version.scala"
  println(s"Generating Version.scala $file")
  IO.write(file,
    s"""package ucesoft.smd
       |object Version {
       | val VERSION = "${version.value}"
       | val SCALA_VERSION = "${scalaVersion.value}"
       | val BUILD_DATE = "${java.time.LocalDateTime.now().format(java.time.format.DateTimeFormatter.ofPattern("dd/MM/yyyy HH:mm:ss"))}"
       |}
       |""".stripMargin)
  Seq(file)
}.taskValue

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.3.1"
libraryDependencies += "com.formdev" % "flatlaf" % "3.0"
libraryDependencies += "org.yaml" % "snakeyaml" % "2.0"
libraryDependencies += "org.jfree" % "jfreechart" % "1.5.4"

Compile / resourceDirectory := baseDirectory.value / "resources"
Compile / scalaSource := baseDirectory.value / "src"
