ThisBuild / scalaVersion := "2.13.6"

val circeV = "0.14.1"

lazy val `sslv-notifier` = (project in file("sslv-notifier"))
  .settings(
    libraryDependencies := Seq(
      "net.ruippeixotog" %% "scala-scraper" % "2.2.1",
      "org.http4s" %% "http4s-blaze-client" % "1.0.0-M23",
      "io.circe" %% "circe-core" % circeV,
      "io.circe" %% "circe-parser" % circeV,
      "io.circe" %% "circe-generic" % circeV,
      "org.typelevel" %% "cats-effect" % "3.1.1"
    ),
    name := "sslv-notifier",
    organization := "io.github.vijexa",
    version := "1.0"
  )

lazy val `apartment-notifier` = (project in file("apartment-notifier"))
  .dependsOn(`sslv-notifier`)
  .settings(libraryDependencies := Seq("org.rogach" %% "scallop" % "4.0.3"))

lazy val root = (project in file(".")).aggregate(`sslv-notifier`)

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
