package com.sutemi.http4scatseffect

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.Uri.Path
import org.http4s._
import org.http4s.implicits._

class HutsSpec extends CatsEffectSuite {
  test("GetHuts with a known hut returns status code 200") {
    for {
      repo <- initHutRepository()
      _ <- assertIO(getHut(repo, "1").map(_.status), Status.Ok)
    } yield ()
  }

  test("GetHuts with a known hut returns a hut") {
    for {
      repo <- initHutRepository()
      _ <- assertIO(getHut(repo, "1").flatMap(_.as[Huts.HutWithId]), Huts.HutWithId("1", "thetest"))
    } yield ()
  }

  test("GetHuts with an unknown hut id returns status code 404") {
    for {
      repo <- initHutRepository()
      _ <- assertIO(getHut(repo, "0").map(_.status), Status.NotFound)
    } yield ()
  }

  test("GetHuts with unknown hut id returns empty body") {
    for {
      repo <- initHutRepository()
      _ <- assertIO(getHut(repo, "0").flatMap(_.as[String]), "")
    } yield ()
  }

  test("add then get returns a 200 status") {
    for {
      repo <- initHutRepository()
      resp <- addHut(repo, Huts.Hut(name = "thename"))
      body <- resp.as[Huts.HutWithId]
      _ <- assertIO(getHut(repo, body.id).map(_.status), Status.Ok)
    } yield ()
  }

  test("life cycle returns correct status codes") {
    for {
      repo <- initHutRepository()
      resp <- addHut(repo, Huts.Hut(name = "somename"))
      addbody <- resp.as[Huts.HutWithId]
      _ <- assertIO(getHut(repo, addbody.id).map(_.status), Status.Ok)
      _ <- assertIO(deleteHut(repo, addbody.id).map(_.status), Status.NoContent)
      _ <- assertIO(getHut(repo, addbody.id).map(_.status), Status.NotFound)
    } yield ()
  }

  test("add and update return correct status codes") {
    for {
      repo <- initHutRepository()
      resp <- addHut(repo, Huts.Hut(name = "somename"))
      addbody <- resp.as[Huts.HutWithId]
      _ <- assertIO(getHut(repo, addbody.id).map(_.status), Status.Ok)
      _ <- assertIO(updateHut(repo, Huts.HutWithId(addbody.id, "someothername")).map(_.status), Status.NoContent)
      updateResp <- getHut(repo, addbody.id)
      updated <- updateResp.as[Huts.HutWithId]
      _ <- assertIO(IO{updated.name}, "someothername")
    } yield ()
  }

  private [this] def initHutRepository(): IO[HutRepository[IO]] = {
    val hutRepo = HutRepository.empty[IO]
    hutRepo.addTestHut("1", Huts.Hut("thetest"))
    IO.pure(hutRepo)
  }

  private[this] def addHut(hutRepo: HutRepository[IO], hut: Huts.Hut): IO[Response[IO]] = {
    val huts = Huts.impl[IO](hutRepo)
    val addHut = Request[IO](Method.POST, new Uri(path = Path.empty / "huts")).withEntity(hut)
    Http4scatseffectRoutes.hutRoutes(huts).orNotFound(addHut)
  }

  private[this] def updateHut(hutRepo: HutRepository[IO], hut: Huts.HutWithId): IO[Response[IO]] = {
    val huts = Huts.impl[IO](hutRepo)
    val updateHut = Request[IO](Method.PUT, new Uri(path = Path.empty / "huts" / hut.id)).withEntity(hut)
    Http4scatseffectRoutes.hutRoutes(huts).orNotFound(updateHut)
  }

  private[this] def getHut(hutRepo: HutRepository[IO], id: String): IO[Response[IO]] = {
    val huts = Huts.impl[IO](hutRepo)
    // todo what do we need to do to get the uri building right?
    val getHut = Request[IO](Method.GET, new Uri(path = Path.empty / "huts" / id))
    Http4scatseffectRoutes.hutRoutes(huts).orNotFound(getHut)
  }

  private[this] def deleteHut(hutRepo: HutRepository[IO], id: String): IO[Response[IO]] = {
    val huts = Huts.impl[IO](hutRepo)
    val deleteHut = Request[IO](Method.DELETE, new Uri(path = Path.empty / "huts" / id))
    Http4scatseffectRoutes.hutRoutes(huts).orNotFound(deleteHut)
  }

}
