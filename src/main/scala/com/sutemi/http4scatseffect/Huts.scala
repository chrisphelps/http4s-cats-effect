package com.sutemi.http4scatseffect

import cats.Monad
import cats.effect.Concurrent
import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.circe._

//import org.http4s.implicits._
//import org.http4s.client.Client
//import org.http4s.client.dsl.Http4sClientDsl
//import org.http4s.circe._
//import org.http4s.Method._


import java.util.UUID
import scala.collection.mutable.ListBuffer

// Huts Algebra
trait Huts[F[_]] {
  def get(id: String): F[Option[Huts.HutWithId]]
  def addHut(hut: Huts.Hut): F[String]
}

object Huts {
  // todo why do I need the summon apply? for test maybe?
  def apply[F[_]](implicit ev: Huts[F]): Huts[F] = ev

  // why does this need to extend AnyVal? What does the case class extend otherwise?
  final case class Hut(name: String) extends AnyVal
  object Hut {
    implicit val hutDecoder: Decoder[Hut] = deriveDecoder[Hut]
    implicit def hutEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, Hut] = jsonOf
    implicit val hutEncoder: Encoder[Hut] = deriveEncoder[Hut]
    implicit def hutEntityEncoder[F[_]]: EntityEncoder[F, Hut] = jsonEncoderOf
  }

  final case class HutWithId(id:String, name: String)
  object HutWithId {
    implicit val hutWithIdDecoder: Decoder[HutWithId] = deriveDecoder[HutWithId]
    implicit def hutWithIdEntityDecoder[F[_]: Concurrent]: EntityDecoder[F, HutWithId] = jsonOf
    implicit val hutWithIdEncoder: Encoder[HutWithId] = deriveEncoder[HutWithId]
    implicit def hutWithIdEntityEncoder[F[_]]: EntityEncoder[F, HutWithId] = jsonEncoderOf
  }

  // todo this will probably want to be a Sync eventually not an Applicative - Sync is Applicative after
  // todo some layers. Which is "lowest power"?
  // todo hm, this feeling like a bit of boilerplate....
  def impl[F[_]](hutRepo: HutRepository[F]): Huts[F] = new Huts[F] {
    def get(id: String): F[Option[Huts.HutWithId]] =
      hutRepo.getHut(id)

    def addHut(hut: Huts.Hut): F[String] =
      hutRepo.addHut(hut)
  }
}

final class HutRepository[F[_]: Monad](private val huts: ListBuffer[Huts.HutWithId]) {

  val makeId: F[String] =
    UUID.randomUUID().toString.pure[F]

  def getCannedHut(id: String): F[Huts.HutWithId] =
    Huts.HutWithId(id, "CannedName").pure[F]


  def getHut(id: String): F[Option[Huts.HutWithId]] =
    huts.find(_.id == id).pure[F]

  def addTestHut(id: String, hut: Huts.Hut): F[String] = {
    huts.addOne(Huts.HutWithId(id, hut.name))
    id.pure[F]
  }

  def addHut(hut: Huts.Hut): F[String] = {
    for {
      uuid <- makeId
      _ <- huts.addOne(Huts.HutWithId(uuid, hut.name)).pure[F]
    } yield uuid
  }

  //  def addHut(hut: Huts.Hut): F[String] =
//    for {
//      uuid <- makeId
//      _ <- huts += Huts.HutWithId(uuid, hut.name).pure[F]
//    } yield uuid

//  def updateHut(hut: Huts.HutWithId): IO[Unit] =
//    for {
//      _ <- IO { huts -= hut }
//      _ <- IO { huts += hut }
//    } yield ()
//
//  def deleteHut(hut: Huts.HutWithId): IO[Unit] =
//    for {
//      _ <- IO {
//        huts.find(_.id == hut.id).map {
//          h => IO {
//            huts -= h
//          }
//        }
//      }
//    } yield ()
}

object HutRepository {
  def empty[F[_]: Monad]: HutRepository[F] = new HutRepository[F](ListBuffer())
}
