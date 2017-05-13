name := "SbtProject"

version := "1.0"

scalaVersion := "2.12.2"

mainClass in assembly := Some("com.kotori316.keisan.Main")

assemblyJarName := s"Keisan-${version.value}.jar"

        