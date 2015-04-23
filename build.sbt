organization := "co.movio"

name := "sbt-tools"

version := "0.1.0"

sbtPlugin := true

publishMavenStyle := false

publishArtifact in Test := false

//publishTo := Some(Resolver.url("repo", new URL("http://"))(Resolver.ivyStylePatterns))

//sbtVersion in Global := "0.13.5"

libraryDependencies ++= Seq(
)

publishTo <<= version { (v: String) ⇒
  val repo = "https://artifactory.movio.co/artifactory/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("movio snapshots" at repo + "libs-snapshot-local")
  else
    Some("movio releases" at repo + "libs-release-local")
}
