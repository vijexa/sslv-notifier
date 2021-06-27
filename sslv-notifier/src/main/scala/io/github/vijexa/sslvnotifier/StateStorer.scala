package io.github.vijexa.sslvnotifier

import better.files._
import cats.effect.kernel.Sync

import File._
import scala.annotation.nowarn

@nowarn("msg=discarded")
object StateStorer {

  private val file: File = root / "var" / "tmp" / "sslv-notifier-temp.txt"

  def saveIds[F[_]: Sync](ids: List[String]): F[Unit] =
    Sync[F].blocking(file.append(ids.mkString("\n")))

  def getIds[F[_]: Sync]: F[List[String]] =
    Sync[F].blocking(file.contentAsString.split("\n").toList)

}
