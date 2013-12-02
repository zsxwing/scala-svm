name            := "scala-svm"

version         := "0.1.0-SNAPSHOT"

organization    := "me.iamzsx"

description     := "A Scala implementation of SVM"

homepage        := Some(url("https://github.com/zsxwing/" + name.value))

licenses        := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

scalaVersion    := "2.10.2"

crossScalaVersions := Seq("2.9.2", "2.10.2")

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "junit" % "junit" % "4.10"
)

