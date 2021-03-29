package com.softwaremill.bootzooka.timecardsystems.models

import com.softwaremill.bootzooka.timecardsystems.Error
import cats.effect.{Blocker, ContextShift, IO}
import cats.implicits.catsSyntaxEitherId
import com.softwaremill.bootzooka.timecardsystems.{CoreOperation, TaskIO}
import doobie.HC
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobie.util.transactor.Transactor
import munit.CatsEffectSuite

import java.time.Duration

object HogeHoge{
  def makeXa(blocker: Blocker)(implicit cs: ContextShift[IO]) = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://34.85.122.210/postgres?currentSchema=public",
    "postgres",
    "doh3aneiqu5jeib4pe7bei2uk4she2cai8laiHa&S7eifeequ2aich3gie&Gohqu5oolan",
    blocker
  )
}
class DbTest extends CatsEffectSuite {


  implicit val ec: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)
  val xa: Transactor[IO] = Transactor.after
    .set(HogeHoge.makeXa(Blocker.liftExecutionContext(ExecutionContexts.synchronous)), HC.rollback)

  test("actionsテーブルのテスト") {
    sql"select* from actions".query.to[List].transact(xa)
  }
  test("companyテーブルのテスト") {
    sql"select* from company".query.to[List].transact(xa)
  }
  test("usersテーブルのテスト") {
    sql"select* from users".query.to[List].transact(xa)
  }

  test("既存のデータでlistTotalTimesが完走する") {
    // select * from actions where company_id = 1 and user_id = 1 limit 2;
    CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1))
  }
  test("既存のデータでlistTotalTimesが期待した値を返す take2") {
    assertEquals(
      CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1).take(2)),
      (
        List(Duration.parse("PT14.977591S").asRight),
        Duration.parse("PT14.977591S")
      )
    )
  }

  test("既存のデータでlistTotalTimesが期待した値を返す take3") {
    assertEquals(
      CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1).take(3)),
      (
        List(Duration.parse("PT14.977591S").asRight),
        Duration.parse("PT14.977591S")
      )
    )
  }

  test("既存のデータでlistTotalTimesが期待した値を返す take4") {
    assertEquals(
      CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1).take(4)),
      (
        List(
          Duration.parse("PT14.977591S").asRight
        ),
        Duration.parse("PT14.977591S")
      )
    )
  }

  test("既存のデータでlistTotalTimesが期待した値を返す take5") {
    assertEquals(
      CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1).take(5)),
      (
        List(
          Duration.parse("PT14.977591S").asRight
        ),
        Duration.parse("PT14.977591S")
      )
    )
  }

  test("既存のデータでlistTotalTimesが期待した値を返す take6") {
    assertEquals(
      CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1).take(6)),
      (
        List(
          Duration.parse("PT14.977591S").asRight
        ),
        Duration.parse("PT14.977591S")
      )
    )
  }

  test("既存のデータでlistTotalTimesが期待した値を返す take30") {
    assertEquals(
      CoreOperation.listTotalTimes(TaskIO.selectFromCompany(xa, 1, 1).take(30)),
      (
        List(
          Duration.parse("PT14.977591S").asRight,
          Duration.parse("PT9.630784S").asRight,
          Duration.parse("PT0S").asRight,
          Error.ContinualSameValueException.asLeft,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT5M57.363098S").asRight,
          Error.ContinualSameValueException.asLeft,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT40.347742S").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT17.510149S").asRight,
          Duration.parse("PT6M5.806911S").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT1M13.59584S").asRight,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT13.037793S").asRight,
          Duration.parse("PT47.798439S").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT6M50.801835S").asRight,
          Duration.parse("PT0S").asRight
        ),
        Duration.parse("PT22M30.870182S")
      )
    )
  }
}
