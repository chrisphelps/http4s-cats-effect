package com.sutemi.http4scatseffect

import cats.effect.{Async, Resource}
import cats.syntax.all._
import com.comcast.ip4s._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger

object Http4scatseffectServer {

  def run[F[_]: Async]: F[Nothing] = {
    for {
      client <- EmberClientBuilder.default[F].build
      helloWorldAlg = HelloWorld.impl[F]
      jokeAlg = Jokes.impl[F](client)

      hutsRepo = HutRepository.empty
      _ <- Resource.pure(hutsRepo.addTestHut("1", Huts.Hut("First Hut")))
      _ <- Resource.pure(hutsRepo.addTestHut("2", Huts.Hut("Second Hut")))
      hutAlg = Huts.impl[F](hutsRepo)

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      httpApp = (
        Http4scatseffectRoutes.helloWorldRoutes[F](helloWorldAlg) <+>
        Http4scatseffectRoutes.jokeRoutes[F](jokeAlg) <+>
        Http4scatseffectRoutes.hutRoutes[F](hutAlg)
      ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      _ <- 
        EmberServerBuilder.default[F]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever
}
