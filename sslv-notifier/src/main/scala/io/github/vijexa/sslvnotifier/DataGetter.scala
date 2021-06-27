package io.github.vijexa.sslvnotifier

import cats.data.EitherT
import cats.effect.kernel.Async
import cats.effect.kernel.Ref
import cats.implicits._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import org.http4s.client.Client

import scala.concurrent.duration._

final class DataGetter[F[_]: Async](
  url: String,
  filterFunction: RowItem => Boolean,
  notifierFunction: NotifierFunction[F],
  client: Client[F]
) {

  private val browser = JsoupBrowser()

  private def getRows: F[Either[String, List[RowItem]]] = {
    val rows: EitherT[F, String, List[RowItem]] = for {
      doc <- EitherT.liftF(Async[F].delay(browser.get(url)))

      _ <- printlnET("")

      tables <- EitherT
        .fromOption[F](
          doc >?> elementList("#page_main tr td form table"),
          s"cannot find main tables on page $doc"
        )
      itemTable <- EitherT.fromOption[F](tables.lift(2), s"cannot find correct table in $tables")

      headerColumns <- EitherT
        .fromOption[F](
          itemTable >?> elementList("#head_line td noindex a"),
          s"cannot find headline in $itemTable"
        )

      columnNames = headerColumns.drop(1).map(_.innerHtml)

      _ <- printlnET(s"Parsed column names: $columnNames")

      rawItems <- EitherT
        .fromOption[F](itemTable >?> elementList("tr"), s"cannot find items in $itemTable")

      (unparsedElements, items) = rawItems
        .drop(1)
        .map(RowItem.parseRow(columnNames.length, columnNames))
        .partition(_.isLeft)

      _ <-
        if (unparsedElements.length > 0)
          printlnET[F, String]("Parsing has failed for following elements:")
        else EitherT.fromEither[F](Right(""))

      _ <- unparsedElements
        .map(x => x.left.map(_.toString).left.getOrElse(""))
        .map(printlnET[F, String](_))
        .sequence

    } yield items.flatMap(_.toOption)

    rows.value
      .flatTap {
        case Left(error) => printlnF(s"ERROR! Page can't be parsed! Reason: $error")
        case Right(items) => printlnF(s"Parsed items: $items")
      }
  }

  private def getPics(item: RowItem): F[Either[String, List[List[Byte]]]] = {
    val pics: EitherT[F, String, List[List[Byte]]] = for {
      _ <- printlnET("getting pics...")

      doc <- EitherT.liftF(Async[F].delay(browser.get(item.url)))

      thumbnailUrlElements <- EitherT.fromOption[F](
        doc >?> elementList(".pic_dv_thumbnail a"),
        "can't find thumbnail <a> elements"
      )

      thumbnailUrls = thumbnailUrlElements.map(_.attr("href"))

      _ <- printlnET(s"thumbnail urls: $thumbnailUrls")

      pics <- EitherT.liftF(
        thumbnailUrls
          .map(url => client.get(url)(r => r.body.compile.toList))
          .sequence
      )

    } yield pics

    pics.value
  }

  private def process(state: Ref[F, List[String]], period: FiniteDuration): F[Unit] = for {
    rowsEither <- getRows.handleError(x => Left(x.toString))

    rows <- rowsEither match {
      case Left(error) => notifierFunction.reportError(error) *> Async[F].delay(List.empty[RowItem])
      case Right(value) => Async[F].delay(value)
    }

    newRows <- state.modify { seenIds =>
      val newRows = rows.filterNot(row => seenIds.contains(row.id))
      val newIds = newRows.map(_.id)
      val newState = seenIds ++ newIds

      (newState, StateStorer.saveIds(newIds) as newRows)
    }.flatten
    _ <- printlnF(s"got ${newRows.length} new rows")

    filteredRows = newRows.filter(filterFunction)
    _ <- printlnF(s"${filteredRows.length} records pass filtering predicate")

    picsEithers <- filteredRows.map(getPics).sequence
    pics <- picsEithers.map {
      case Left(error) =>
        printlnF(s"error occured when fetching pics: $error") *> Async[F].delay(
          List.empty[List[Byte]]
        )
      case Right(value) => Async[F].delay(value)
    }.sequence

    rowsWithPics = filteredRows.zip(pics).map { case (item, p) =>
      RowItemWithPics(item, p)
    }

    _ <-
      if (filteredRows.length > 0)
        printlnF("notifying user...") *> rowsWithPics.map(x => notifierFunction.notify(x)).sequence
      else Async[F].unit

    _ <- Async[F].sleep(period)
    _ <- process(state, period)
  } yield ()

  def startProcessing(period: FiniteDuration): F[Unit] = for {
    state <- Ref.of[F, List[String]](List.empty)
    recoveredIds <- StateStorer.getIds
    _ <- state.set(recoveredIds.toList)
    _ <- printlnF(s"recovered ids: $recoveredIds")
    _ <- notifierFunction.reportError("sslv-notifier has started. It may have crashed.")
    _ <- process(state, period)
  } yield ()
}
