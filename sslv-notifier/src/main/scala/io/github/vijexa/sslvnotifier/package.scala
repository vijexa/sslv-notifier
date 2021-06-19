package io.github.vijexa

import cats.data.EitherT
import cats.effect.kernel.Sync

package object sslvnotifier {
  def printlnF[FF[_]: Sync](str: String): FF[Unit] = Sync[FF].delay(println(str))
  def printlnET[FF[_]: Sync, A](str: String): EitherT[FF, A, Unit] =
    EitherT.liftF(printlnF[FF](str))
}
