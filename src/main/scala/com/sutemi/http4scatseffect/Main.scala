package com.sutemi.http4scatseffect

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {
  val run = Http4scatseffectServer.run[IO]
}
