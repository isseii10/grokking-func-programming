val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "grokking-func-programming",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.typelevel" %% "cats-effect" % "3.5.3",
      "org.typelevel" %% "cats-core" % "2.10.0",
      "co.fs2" %% "fs2-core" % "3.2.7",
      "org.apache.jena" % "apache-jena-libs" % "4.10.0",
      "org.apache.jena" % "jena-fuseki-main" % "4.10.0",
      "org.slf4j" % "slf4j-nop" % "2.0.12"
    )
  )
