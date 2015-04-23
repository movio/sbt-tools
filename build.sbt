organization := "co.movio"

name := "sbt-tools"

version := "0.2.0-SNAPSHOT"

sbtPlugin := true

publishMavenStyle := false

publishArtifact in Test := false

publishTo := Some(Resolver.url("movio", new URL("https://artifactory.movio.co/artifactory/sbt-plugins-release-local"))(Resolver.ivyStylePatterns))
