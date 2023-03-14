package com.sutemi.http4scatseffect

import cats.effect.{Concurrent, Sync}
import cats.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import org.http4s._
import org.http4s.circe._
import org.typelevel.log4cats.slf4j.Slf4jLogger


//import org.http4s.implicits._
//import org.http4s.client.Client
//import org.http4s.client.dsl.Http4sClientDsl
//import org.http4s.circe._
//import org.http4s.Method._


import java.util.UUID
import scala.collection.mutable.ListBuffer

// Huts Algebra
trait Huts[F[_]] {
  def getAll: F[List[Huts.HutWithId]]
  def get(id: String): F[Option[Huts.HutWithId]]
  def add(hut: Huts.Hut): F[Huts.HutWithId]
  def update(id: String, hut: Huts.HutWithId): F[Unit]
  def delete(id: String): F[Option[Huts.HutWithId]]
}

object Huts {
  // needed for the tests
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

  // todo hm, this feeling like a bit of boilerplate....
  def impl[F[_]: Sync](hutRepo: HutRepository[F]): Huts[F] = new Huts[F] {

    def getAll: F[List[Huts.HutWithId]] = {
      for {
        logger <- Slf4jLogger.create[F]
        _ <- logger.info(s"Get all huts")
        huts <- hutRepo.getAll
      } yield huts
    }

    def get(id: String): F[Option[Huts.HutWithId]] = {
      for {
        logger <- Slf4jLogger.create[F]
        _ <- logger.info(s"Get hut with id $id")
        hut <- hutRepo.getHut(id)
      } yield hut
    }

    def add(hut: Huts.Hut): F[Huts.HutWithId] = {
      for {
        logger <- Slf4jLogger.create[F]
        _ <- logger.info(s"Add hut with name ${hut.name}")
        hut <- hutRepo.addHut(hut)
      } yield hut
    }

    def update(id: String, hut: Huts.HutWithId): F[Unit] = {
      for {
        logger <- Slf4jLogger.create[F]
        _ <- logger.info(s"Update hut with id $id")
        _ <- hutRepo.updateHut(hut)
      } yield ()
    }

    def delete(id: String): F[Option[Huts.HutWithId]] = {
      for {
        logger <- Slf4jLogger.create[F]
        _ <- logger.info(s"Delete hut with id $id")
        hut <- hutRepo.deleteHut(id)
      } yield hut
    }
  }
}

final class HutRepository[F[_]: Sync](private val huts: ListBuffer[Huts.HutWithId]) {

  val makeId: F[String] =
    UUID.randomUUID().toString.pure[F]

  def getAll: F[List[Huts.HutWithId]] =
    huts.result().pure[F]

  def getCannedHut(id: String): F[Huts.HutWithId] =
    Huts.HutWithId(id, "CannedName").pure[F]

  def getHut(id: String): F[Option[Huts.HutWithId]] = {
    for {
      logger <- Slf4jLogger.create[F]
      _ <- logger.info(s"GetHut for $id")
      hut <- huts.find(_.id == id).pure[F]
    } yield hut
  }

  def addTestHut(id: String, hut: Huts.Hut): F[String] = {
    huts.addOne(Huts.HutWithId(id, hut.name))
    id.pure[F]
  }

  def addHut(hut: Huts.Hut): F[Huts.HutWithId] = {
    for {
      uuid <- makeId
      _ <- huts.addOne(Huts.HutWithId(uuid, hut.name)).pure[F]
    } yield Huts.HutWithId(uuid, hut.name)
  }

  // todo should this have something else for success/fail? Or does F capture that?
  def updateHut(hut: Huts.HutWithId): F[Unit] =
    for {
      logger <- Slf4jLogger.create[F]
      _ <- logger.info(s"Updating hut. Huts repo: $huts")
      toremove <- huts.find(_.id == hut.id).pure[F]
      // todo don't just assume the option here is Some
      _ <- (huts -= toremove.get).pure[F]
      _ <- (huts += hut).pure[F]
      _ <- logger.info(s"Updated hut. Huts repo: $huts")
    } yield ()


  // todo should I simplify the naming here?
  // todo is F[Option[Huts.HutWithId]] the best way to capture the found/deleted stuff?
  def deleteHut(id: String): F[Option[Huts.HutWithId]] = {
    huts.find(_.id == id)
      .map { h =>
        // todo this does feel like this is doing side effects and wrapping that in the pure[F]
        // todo will see what happens when we move to more real repository
        huts -= h
        h
      }.pure[F]
  }
}

object HutRepository {
  def empty[F[_]: Sync]: HutRepository[F] = new HutRepository[F](ListBuffer())
}

