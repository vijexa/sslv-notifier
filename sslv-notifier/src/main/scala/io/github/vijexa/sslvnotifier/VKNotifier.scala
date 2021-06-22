package io.github.vijexa.sslvnotifier

import cats.data.EitherT
import cats.effect.kernel._
import cats.implicits._
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.parser.decode
import org.http4s._
import org.http4s.circe.decodeUri
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.headers._
import org.http4s.multipart._
import org.typelevel.ci._

import java.net.URLEncoder

class VKNotifier[F[_]: Async](
  accessToken: String,
  ownerId: String,
  peerId: String,
  client: Client[F],
  itemPrinter: RowItem => String = VKNotifier.defaultItemPrinter("")
) extends NotifierFunction[F]
    with Http4sClientDsl[F] {

  case class SendMessageR(response: Long)
  implicit val sendMessageResponseDecoder: Decoder[SendMessageR] = deriveDecoder

  case class PhotoGetMessagesUploadServerRResponse(upload_url: Uri)
  implicit val photoGetMessagesUploadServerResponseDecoder =
    deriveDecoder[PhotoGetMessagesUploadServerRResponse]
  case class PhotoGetMessagesUploadServerR(response: PhotoGetMessagesUploadServerRResponse)
  implicit val photoGetMessagesUploadServerDecoder =
    deriveDecoder[PhotoGetMessagesUploadServerR]

  case class PhotoUploadR(photo: String, hash: String, server: Long)
  implicit val photoUploadRDecoder =
    deriveDecoder[PhotoUploadR]

  case class PhotoSaveMessagesPhotoRResponse(id: Long)
  implicit val photoSaveMessagesPhotoRResponseDecoder =
    deriveDecoder[PhotoSaveMessagesPhotoRResponse]
  case class PhotoSaveMessagesPhotoR(response: Array[PhotoSaveMessagesPhotoRResponse])
  implicit val photoSaveMessagesPhotoRDecoder =
    deriveDecoder[PhotoSaveMessagesPhotoR]

  protected def generateUri(method: String, args: Map[String, String] = Map.empty): String =
    s"https://api.vk.com/method/$method?" +
      s"v=5.131&" +
      s"access_token=${accessToken}&" +
      args
        .map { case (name, value) =>
          s"$name=${URLEncoder.encode(value, "UTF-8")}"
        }
        .mkString("&")

  protected def decodeResponse[R: Decoder, RR](errMap: io.circe.Error => String, map: R => RR)(
    resp: Response[F]
  ) =
    resp.bodyText.compile.toList.map(body =>
      decode[R](body.mkString)
        .map(map)
        .leftMap(err => s"body: $body\nerror: ${errMap(err)}")
    )

  protected def makeGetRequest[R: Decoder, RR](
    uri: String,
    errMap: io.circe.Error => String,
    map: R => RR
  ): F[Either[String, RR]] = client.get(uri)(decodeResponse(errMap, map))

  protected def sendMessage(
    message: String,
    args: Map[String, String] = Map.empty
  ): F[Either[String, Long]] =
    makeGetRequest[SendMessageR, Long](
      generateUri(
        "messages.send",
        Map("message" -> message, "peer_id" -> peerId, "random_id" -> "0") ++ args
      ),
      _.toString,
      _.response
    )

  protected def photoGetMessagesUploadServer: F[Either[String, Uri]] =
    makeGetRequest[PhotoGetMessagesUploadServerR, Uri](
      generateUri("photos.getMessagesUploadServer"),
      _.toString,
      _.response.upload_url
    )

  protected def photoSaveMessagesPhoto(
    server: String,
    photo: String,
    hash: String
  ): F[Either[String, String]] =
    makeGetRequest[PhotoSaveMessagesPhotoR, String](
      generateUri(
        "photos.saveMessagesPhoto",
        Map("server" -> server, "photo" -> photo, "hash" -> hash)
      ),
      _.toString,
      _.response.head.id.toString
    )

  private def generateMultipart(pic: List[Byte]): Multipart[F] = Multipart[F](
    Vector(
      Part(
        Headers(
          `Content-Disposition`("form-data", Map(ci"name" -> "photo", ci"filename" -> "photo.jpg"))
        ),
        fs2.Stream.emits(pic)
      )
    )
  )

  private def uploadPhotos(photos: List[List[Byte]]): F[Either[String, List[String]]] = {
    val photoIdsEitherT: EitherT[F, String, List[String]] = for {
      uploadServer <- EitherT(photoGetMessagesUploadServer)

      _ <- printlnET(s"upload server: $uploadServer")

      requests = photos.map { pic =>
        val multipart = generateMultipart(pic)

        Method
          .POST(multipart, uploadServer)
          .withHeaders(multipart.headers)
      }

      uploadedResponsesEithers <- EitherT
        .liftF(
          requests
            .map(req =>
              client.run(req).use(decodeResponse[PhotoUploadR, PhotoUploadR](_.toString, identity))
            )
            .sequence
        )

      savedResponsesEithers <- EitherT.liftF(
        uploadedResponsesEithers
          .map[F[Option[Either[String, String]]]] {
            case Left(error) =>
              printlnF(s"error when uploading photo: $error") as None
            case Right(value) =>
              photoSaveMessagesPhoto(value.server.toString, value.photo, value.hash).map(Some(_))
          }
          .sequence
          .map(_.flatten)
      )

      photoIds <- EitherT.liftF(
        savedResponsesEithers
          .map[F[Option[String]]] {
            case Left(error) => printlnF(s"error when saving photo: $error") as None
            case Right(value) =>
              printlnF(s"photo $value saved and uploaded successfully") as Some(value)
          }
          .sequence
          .map(_.flatten)
      )

    } yield photoIds

    photoIdsEitherT.value
  }

  def notify(item: RowItemWithPics): F[Unit] = {

    for {
      photoIds <- uploadPhotos(item.pics)
        .handleError(err =>
          s"photo uploading completely blew up with following exception: $err".asLeft
        )
        .flatMap[Option[List[String]]] {
          case Left(error) => printlnF(s"photo uploading failed completely: $error") as None
          case Right(value) => Sync[F].delay(Some(value))
        }

      response <- sendMessage(
        itemPrinter(item.item),
        photoIds match {
          case Some(ids) => Map("attachment" -> ids.map(id => s"photo${ownerId}_$id").mkString(","))
          case None => Map()
        }
      )

      _ <- if (response.isLeft) reportError(response.left.getOrElse("")) else Async[F].unit
    } yield ()
  }

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
      s"New ad that fits $yourString predicate just was posted\n${dataPrinter(item.data)}\n${item.url}"
  }
}
