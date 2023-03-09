package com.sutemi.http4scatseffect

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.Uri.Path
import org.http4s._
import org.http4s.implicits._


class HutsSpec extends CatsEffectSuite {
  test("GetHuts with a known hut returns status code 200") {
    assertIO(retGetHut("1").map(_.status), Status.Ok)
  }

  test("GetHuts with a known hut returns a hut") {
    assertIO(retGetHut("1").flatMap(_.as[Huts.HutWithId]), Huts.HutWithId("1", "thetest"))
  }

  test("GetHuts with an unknown hut id returns status code 404") {
    assertIO(retGetHut("0").map(_.status), Status.NotFound)
  }

  test("GetHuts with unknown hut id returns empty body") {
    assertIO(retGetHut("0").flatMap(_.as[String]), "")
  }

  // todo this suggests I should do something different to pass in the repo.
  // todo maybe do that in another pass later (getAll or delete perhaps)
  private[this] def retGetHut(id: String): IO[Response[IO]] = {
    val hutRepo = HutRepository.empty[IO]
    hutRepo.addTestHut("1", Huts.Hut("thetest"))
    val huts = Huts.impl[IO](hutRepo)
    // todo what do we need to do to get the uri building right?
    val getHut = Request[IO](Method.GET, new Uri(path = Path.empty / "huts" / id))
    Http4scatseffectRoutes.hutRoutes(huts).orNotFound(getHut)
  }
}
