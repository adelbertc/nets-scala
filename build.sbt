name := "nets"

organization := "com.adelbertc"

description := "Simple graph library."

version := "0.0.1"

scalaVersion := "2.10.3"

licenses += ("BSD-3-Clause", url("http://www.opensource.org/licenses/BSD-3-Clause"))

resolvers ++= Seq(
  "Sonatype Public"     at "https://oss.sonatype.org/content/groups/public",
  "Sonatype Releases"   at "https://oss.sonatype.org/content/repositories/releases",
  "tpolecat"            at "https://dl.bintray.com/tpolecat/maven"
)

addCompilerPlugin("org.brianmckenna" %% "wartremover" % "0.7")

libraryDependencies ++= Seq(
  "org.scalaz"      %% "scalaz-core"    % "7.1.0-M4",
  "org.scalaz"      %% "scalaz-effect"  % "7.1.0-M4",
  "org.spire-math"  %% "spire"          % "0.7.1",
  "org.tpolecat"    %% "atto"           % "0.1"
)

scalacOptions ++= Seq(
  "-deprecation", 
  "-encoding", "UTF-8",
  "-feature", 
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xlog-reflective-calls",
  "-Yno-adapted-args",
  "-Ywarn-all",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard",
  "-P:wartremover:traverser:org.brianmckenna.wartremover.warts.Unsafe"
)
