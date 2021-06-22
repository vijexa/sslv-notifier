package io.github.vijexa.apartmentnotifier

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Resource
import io.github.vijexa.sslvnotifier.DataGetter
import io.github.vijexa.sslvnotifier.VKNotifier
import org.http4s.blaze.client.BlazeClientBuilder
import org.rogach.scallop.ScallopConf

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

class Config(arguments: Seq[String]) extends ScallopConf(arguments) {
  val accessToken = opt[String](required = true)
  val ownerId = opt[String](required = true)
  val peerId = opt[String](required = true)
  val userId = opt[String]()
  val period = opt[Int]()

  val minCost = opt[Int]()
  val maxCost = opt[Int]()

  val minArea = opt[Int]()
  val maxArea = opt[Int]()
  verify()
}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val numRegex = """.*?(\d+).*""".r
    val config = new Config(args)
    val timeout = 3600.seconds

    val resource = for {
      client <- BlazeClientBuilder[IO](global)
        .withResponseHeaderTimeout(timeout)
        .withConnectTimeout(timeout)
        .withIdleTimeout(timeout)
        .withRequestTimeout(timeout)
        .resource

      _ <- Resource.eval(
        new DataGetter[IO](
          "https://www.ss.lv/ru/real-estate/flats/riga/centre/today/hand_over/",
          item => {
            val data = item.data

            val decision = for {
              costStr <- data.get("Цена")
              cost <- numRegex.unapplySeq(costStr).flatMap(_.headOption).flatMap(_.toIntOption)
              if cost <= config.maxCost.getOrElse(Int.MaxValue) && cost >= config.minCost.getOrElse(
                0
              )

              m2Str <- data.get("m2")
              m2 <- m2Str.toIntOption
              if m2 >= config.minArea.getOrElse(0) && m2 <= config.maxArea.getOrElse(Int.MaxValue)

              // https://github.com/scala/bug/issues/10287
              _ = cost
            } yield true

            decision.getOrElse(false)
          },
          new VKNotifier[IO](
            config.accessToken(),
            config.ownerId(),
            config.peerId(),
            client,
            VKNotifier.defaultItemPrinter(config.userId.getOrElse(""))
          ),
          client
        ).startProcessing(config.period.getOrElse(30).seconds)
      )
    } yield ExitCode.Success

    resource.use(IO(_))
  }
}
