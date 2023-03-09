package com.sutemi.http4scatseffect

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl

object Http4scatseffectRoutes {

  def jokeRoutes[F[_]: Sync](J: Jokes[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "joke" =>
        for {
          joke <- J.get
          resp <- Ok(joke)
        } yield resp
    }
  }

  def helloWorldRoutes[F[_]: Sync](H: HelloWorld[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
      case GET -> Root / "hello" / name =>
        for {
          greeting <- H.hello(HelloWorld.Name(name))
          resp <- Ok(greeting)
        } yield resp
    }
  }

  def hutRoutes[F[_]: Concurrent](H: Huts[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F]{}
    import dsl._
    HttpRoutes.of[F] {
          // todo get all huts
      case GET -> Root / "huts" / id =>
        for {
          hut <- H.get(id)
          resp <- hut.fold(NotFound())(Ok(_))
        } yield resp
      case req @ POST -> Root / "huts" =>
        for {
          hut <- req.as[Huts.Hut]
          uuid <- H.addHut(hut)
          resp <- Created(uuid)
        } yield resp
        // todo add/update hut
        // todo delete hut
    }
  }
}