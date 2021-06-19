package io.github.vijexa.sslvnotifier

import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element

final case class RowItem(id: String, url: String, data: Map[String, String])

object RowItem {
  def parseRow(columnCount: Int, columns: List[String])(item: Element): Either[Element, RowItem] = {
    val parsedOpt = for {
      valuesRaw <- item >?> elementList("td")
      values = valuesRaw
        .takeRight(columnCount)
        // remove <b></b> from premium ads
        .map(_.innerHtml.replaceAll("<b>|</b>", ""))

      url <- item >?> element(".msg2 a")
    } yield RowItem(item.attr("id"), "https://ss.lv" + url.attr("href"), columns.zip(values).toMap)

    parsedOpt.fold[Either[Element, RowItem]](Left(item))(Right(_))
  }
}
