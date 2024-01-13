
name := "Sega-MD"

version := "0.9"

scalaVersion := "3.3.1"

scalacOptions ++= Seq(
  "-language:postfixOps",
  "-encoding","ISO-8859-1",
  "-deprecation"
)

libraryDependencies += "com.fifesoft" % "rsyntaxtextarea" % "3.3.1"
libraryDependencies += "com.formdev" % "flatlaf" % "3.0"
libraryDependencies += "org.yaml" % "snakeyaml" % "2.0"

Compile / resourceDirectory := baseDirectory.value / "resources"
Compile / scalaSource := baseDirectory.value / "src"
