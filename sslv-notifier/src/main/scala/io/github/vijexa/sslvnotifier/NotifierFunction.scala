package io.github.vijexa.sslvnotifier

import cats.effect.kernel.Async
import scala.annotation.nowarn

@nowarn
abstract class NotifierFunction[F[_]: Async] {
  def notify(item: RowItemWithPics): F[Unit]
  def reportError(error: String): F[Unit]
}
