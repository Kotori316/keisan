name := "SbtProject"

version := "2.0"

scalaVersion := "2.12.2"

mainClass in assembly := Some("com.kotori316.keisan.Main")

assemblyJarName := s"Keisan-${version.value}.jar"

        