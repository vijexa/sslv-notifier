package io.github.vijexa.sslvnotifier

import cats.effect.kernel.Async
import cats.implicits._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import org.http4s.client.Client

import java.net.URLEncoder

class VKNotifier[F[_]: Async](
  accessToken: String,
  peerId: String,
  client: Client[F],
  itemPrinter: RowItem => String = VKNotifier.defaultItemPrinter("")
) extends NotifierFunction[F] {

  case class SendMessageResponse(response: Long)
  implicit val decoder: Decoder[SendMessageResponse] = deriveDecoder

  protected def generateUri(method: String, args: Map[String, String] = Map.empty): String =
    s"https://api.vk.com/method/$method?" +
      s"v=5.92&" +
      s"access_token=${accessToken}&" +
      args
        .map { case (name, value) =>
          s"$name=${URLEncoder.encode(value, "UTF-8")}"
        }
        .mkString("&")

  protected def makeGetRequest[R: Decoder, RR](
    uri: String,
    errMap: io.circe.Error => String,
    map: R => RR
  ): F[Either[String, RR]] = client.get(uri)(r =>
    r.bodyText.compile.toList.map(body =>
      decode[R](body.mkString)
        .map(map)
        .leftMap(err => s"body: $body\nerror: ${errMap(err)}")
    )
  )

  protected def sendMessage(
    message: String,
    args: Map[String, String] = Map.empty
  ): F[Either[String, Long]] =
    makeGetRequest[SendMessageResponse, Long](
      generateUri(
        "messages.send",
        Map("message" -> message, "peer_id" -> peerId, "random_id" -> "0") ++ args
      ),
      err => err.toString,
      resp => resp.response
    )

  def notify(item: RowItem): F[Unit] = for {
    response <- sendMessage(itemPrinter(item))
    _ <- if (response.isLeft) reportError(response.left.getOrElse("")) else Async[F].unit
  } yield ()

  def reportError(error: String): F[Unit] = for {
    _ <- sendMessage(s"Unexpected error encountered:\n$error")
  } yield ()
}

object VKNotifier {
  val dataPrinter: Map[String, String] => String = _.map { case (columnName, value) =>
    s"$columnName: $value"
  }.mkString("\n")

  def defaultItemPrinter(userId: String): RowItem => String = {
    val yourString =
      if (userId != "") s"[id$userId|your]"
      else "your"

    item =>
      s"New ad that fits $yourString predicate was just posted\n${dataPrinter(item.data)}\n${item.url}"
  }
}
