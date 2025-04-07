import sbt._

object Dependencies {

  lazy val ZioVersion = "2.1.15"

  lazy val ScalaTest = "org.scalatest" %% "scalatest" % "3.2.19"
  lazy val CatsCore = "org.typelevel" %% "cats-core" % "2.13.0"
  lazy val CatsEffect = "org.typelevel" %% "cats-effect" % "3.4.5"
  lazy val ZIO: Seq[ModuleID] = Seq("dev.zio" %% "zio" % ZioVersion,
                     "dev.zio" %% "zio-test" % ZioVersion  % Test,
                     "dev.zio" %% "zio-test-sbt" % ZioVersion  % Test,
                     "dev.zio" %% "zio-test-magnolia" % ZioVersion % Test)
  lazy val ZioConfig: Seq[ModuleID] = Seq(
    "dev.zio" %% "zio-config" % "4.0.2",
    "dev.zio" %% "zio-config-magnolia" % "4.0.2",
    "dev.zio" %% "zio-config-typesafe" % "4.0.2"
  )

}
