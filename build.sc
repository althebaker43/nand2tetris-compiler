import mill._
import mill.scalalib._

object compiler extends ScalaModule {

  def scalaVersion = "3.8.1"

  object test extends ScalaTests with TestModule.Munit {
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::1.0.1"
    )
  }
}

