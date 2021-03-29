package com.softwaremill.bootzooka.timecardsystems.models

import cats.data.NonEmptyList
import cats.implicits._
import com.softwaremill.bootzooka.timecardsystems.CoreOperation._
import com.softwaremill.bootzooka.timecardsystems.EventAction._
import com.softwaremill.bootzooka.timecardsystems.UserAlgorithm._
import com.softwaremill.bootzooka.timecardsystems._
import com.softwaremill.bootzooka.timecardsystems.applyAlgorithmSupports._

import java.time.{Duration, LocalDateTime, LocalTime}

class TaskSpec extends munit.FunSuite {
  test("仕事を初めて1分後に終わった場合のstateは現在何もしていませんになる") {
    val actions: List[Actions] =
      Actions(0, 1, 1, Start, LocalDateTime.of(2021, 2, 22, 16, 51, 0, 0)) ::
        Actions(0, 1, 1, Finish, LocalDateTime.of(2021, 2, 22, 16, 52, 0, 0)) ::
        Nil
    assertEquals(CoreOperation.identifyMyStateAt(actions), "何もしていません")
  }

  //  test("最初のTDD") {
  //    val actions: List[Actions] =
  //      Actions(0, 1, 1, Start, LocalDateTime.of(2021, 2, 22, 16, 51, 0, 0)) ::
  //        Actions(0, 1, 1, Finish, LocalDateTime.of(2021, 2, 22, 16, 52, 0, 0)) ::
  //        Nil
  //    assertEquals(CoreOperation.firstTDDMyFunction(actions), 2)
  //  }

  test("タイマーの役割を果たすのか否か！？") {
    val before = LocalDateTime.of(2000, 2, 20, 23, 0, 0, 0)
    val after = LocalDateTime.of(2000, 2, 20, 23, 45, 3, 0)
    assertEquals(Duration.between(before, after), Duration.parse("PT45M3S"))
  }

  test("タイマーの役割を果たすのか否か2！？") {
    val before = LocalDateTime.of(2000, 1, 1, 0, 0, 0, 0)
    val after = LocalDateTime.of(2000, 1, 1, 1, 0, 0, 0)
    assertEquals(Duration.between(before, after), Duration.parse("PT1H"))
  }

  test("小計を計算をするqqqqq") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(2))
        //        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        //        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50))
      )
    val (startPos, finishPos, _, _) = firstPositions(actions)
    val periodDurations = subtotals(actions, startPos, finishPos).map {
      case Left(_)         => Duration.ZERO
      case Right(duration) => duration
    }
    assertEquals(
      totalTimeOfOneProject(periodDurations),
      Duration.parse("PT2M")
    )
  }

  test("小計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    val lastpos = CoreOperation.lastPositions(actions)
    assertEquals(
      CoreOperation.subtotals(actions, lastpos._1, lastpos._2),
      List(
        Duration.parse("PT10M").asRight,
        Duration.parse("PT0S").asRight,
        Duration.parse("PT30M").asRight,
        Duration.parse("PT0S").asRight
      )
    )
  }

  test("小計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    val lastpos = CoreOperation.lastPositions(actions)
    assertEquals(
      CoreOperation.subtotals(actions, lastpos._1, lastpos._2),
      List(
        Duration.parse("PT10M").asRight,
        Error.ContinualSameValueException.asLeft,
        Duration.parse("PT0S").asRight,
        Duration.parse("PT40M").asRight
      )
    )
  }

  test("小計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    val lastpos = CoreOperation.lastPositions(actions)
    assertEquals(
      CoreOperation.subtotals(actions, lastpos._1, lastpos._2),
      List(
        Duration.parse("PT10M").asRight,
        Error.ContinualSameValueException.asLeft,
        Duration.parse("PT0S").asRight,
        Error.ContinualSameValueException.asLeft
      )
    )
  }

  test("小計を計算をする(20時にrestStartしたまま寝落ち。9時出社。") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.Start, ldt.plusMinutes(780))
      )
    val lastpos = CoreOperation.lastPositions(actions)
    assertEquals(CoreOperation.subtotals(actions, lastpos._1, lastpos._2), Nil)
  }

  test("現在状況を特定する") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    assertEquals(CoreOperation.identifyMyStateAt(actions), "何もしていません")
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight
        ),
        Duration.parse("PT320M")
      )
    )
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Duration.parse("PT30M").asRight,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight
        ),
        Duration.parse("PT270M")
      )
    )
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Duration.parse("PT30M").asRight,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Error.NoStartRestException.asLeft,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight
        ),
        Duration.parse("PT240M")
      )
    )
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Error.FirstElementException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Error.NoStartRestException.asLeft,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight
        ),
        Duration.parse("PT260M")
      )
    )
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(30 + 20)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(30 + 20 + 50)),
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(30))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Error.FirstElementException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Duration.parse("PT30M").asRight,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT50M").asRight,
          Error.NoStartRestException.asLeft,
          Error.ContinualSameValueException.asLeft,
          Duration.parse("PT50M").asRight
//          Duration.parse("PT30M").asRight
        ),
        Duration.parse("PT180M")
      )
    )
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.RestStart, ldt),
        Actions(1, 1, 1, EventAction.Finish, ldt),
        Actions(1, 1, 1, EventAction.Start, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.Start, ldt.plusMinutes(10 + 20 + 30))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Error.FirstElementException.asLeft,
          Duration.parse("PT0S").asRight,
          Duration.parse("PT20M").asRight
        ),
        Duration.parse("PT20M")
      )
    )
  }

  test("累計を計算をする") {
    val ldt = LocalDateTime.of(2021, 2, 24, 12, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Finish, ldt),
        Actions(1, 1, 1, EventAction.Start, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.Start, ldt.plusMinutes(10 + 20 + 30))
      )
    assertEquals(
      CoreOperation.listTotalTimes(actions),
      (
        List(
          Error.FirstElementException.asLeft,
          Duration.parse("PT20M").asRight
        ),
        Duration.parse("PT20M")
      )
    )
  }

  //   Option
  //   Either[String, 小計].right.get => Some(小計) or None, Either[String, 小計].left.get => Some(String) or None
  //   Either[(AmbiguousStart, AmbiguousFinish, String), 小計]

  test("小計を計算をする(20時にrestStartしたまま寝落ち。9時出社。") {
    val actions = Nil
    assertEquals(CoreOperation.listTotalTimes(actions), (Nil, Duration.ZERO))
  }

  test("showDurationのフォーマットをテストする") {
    assertEquals(showDuration(Duration.ZERO), "00:00:00:000")
  }

  test("隣あう二数が同じになっているところを含むかのテスト1") {
    assertEquals(isExistSameValue(List(1, 2, 2, 3, 4)), true)
  }

  test("隣あう二数が同じになっているところを含むかのテスト2") {
    assertEquals(isExistSameValue(List(1, 2, 5, 3, 5)), false)
  }

  test("隣あう二数が同じになっているところを含むかのテスト3") {
    assertEquals(isExistSameValue(Nil), false)
  }

  test("隣あう二数が同じになっているところを含むかのテスト3-1") {
    assertEquals(isExistSameValue(List(1)), false)
  }

  test("隣あう二数が同じになっているところを含むかのテストEventAction") {
    assertEquals(isExistSameValue(List(EventAction.Start, EventAction.Finish)), false)
  }

  test("隣あう二数が同じになっているところを含むかのテストEventAction") {
    assertEquals(isExistSameValue(List(EventAction.RestStart, EventAction.RestFinish, EventAction.RestFinish)), true)
  }

  test("範囲指定してその中から累計を算出する") {
    val ldt = LocalDateTime.of(2021, 1, 1, 23, 20, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    assertEquals(
      takeTotalTimeFromFreeSpan(actions, (ldt.plusMinutes(-10), ldt.plusMinutes(100)))._1,
      List(
        Duration.parse("PT10M").asRight,
        Duration.parse("PT0S").asRight,
        Duration.parse("PT30M").asRight
      )
    )
  }

  test("範囲指定してその中から累計を算出する") {
    val ldt = LocalDateTime.of(2021, 1, 1, 23, 20, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    assertEquals(
      takeTotalTimeFromFreeSpan(actions, (ldt.plusMinutes(-10), ldt.plusMinutes(101)))._1,
      List(
        Duration.parse("PT10M").asRight,
        Duration.parse("PT0S").asRight,
        Duration.parse("PT30M").asRight,
        Duration.parse("PT0M").asRight
      )
    )
  }

  test("範囲指定してその中から累計を算出する") {
    val ldt = LocalDateTime.of(2021, 1, 1, 23, 20, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10)),
        Actions(1, 1, 1, EventAction.RestFinish, ldt.plusMinutes(10 + 20)),
        Actions(1, 1, 1, EventAction.RestStart, ldt.plusMinutes(10 + 20 + 30)),
        Actions(1, 1, 1, EventAction.Finish, ldt.plusMinutes(10 + 20 + 30 + 40))
      )
    assertEquals(
      takeTotalTimeFromFreeSpan(actions, (ldt.plusMinutes(-10), ldt.plusMinutes(101)))._2,
      Duration.parse("PT40M")
    )
  }

  test("デフォルトアルゴリズムで稼働時間を正しく計算する") {
    val ldt = LocalDateTime.of(2021, 1, 1, 21, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt), // 1/1 18:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8)) // 1/2 02:00
      )
    val borderTimes = None
    val upperTimes = Nil
    assertEquals(
      applyAlgorithm(actions, borderTimes, upperTimes, Default, (LocalDateTime.MIN, LocalDateTime.MAX) :: Nil),
      List(
        (LocalDateTime.parse("2021-01-01T00:00:00"), Duration.parse("PT8H"))
      )
    )
  }

  test("フォワードアルゴリズムで稼働期間を正しく計算する 01:00:00.000_000_000 ~ 00:59:59.999_999_999") {
    val ldt = LocalDateTime.of(2021, 1, 1, 18, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt), // 1/1 18:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8)) // 1/2 02:00
      )
    val borderTimes = NonEmptyList
      .one(BorderTimes(1, 1, 1, LocalTime.parse("01:00:00"), LocalDateTime.MIN, LocalDateTime.MIN)).some
    val upperTimes = Nil
    assertEquals(
      applyAlgorithm(actions, borderTimes, upperTimes, Forward, (LocalDateTime.MIN, LocalDateTime.MAX) :: Nil),
      List(
        (LocalDateTime.parse("2021-01-01T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-02T00:00:00"), Duration.parse("PT1H"))
      )
    )
  }

  test("フォワードアルゴリズムで稼働期間を正しく計算する") {
    val ldt = LocalDateTime.of(2021, 1, 1, 22, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt), // 1/1 22:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(7)) // 1/2 05:00
      )
    val borderTimes = NonEmptyList
      .one(BorderTimes(1, 1, 1, LocalTime.parse("00:00:00"), LocalDateTime.MIN, LocalDateTime.MIN)).some
    val upperTimes = List(
      UpperTimes(
        1,
        1,
        1,
        LocalTime.MIN, //23:59:59.999_999_999
        LocalDateTime.MIN, //createdAt
        LocalDateTime.MIN //from
      )
    )
    assertEquals(
      applyAlgorithm(actions, borderTimes, upperTimes, Forward, (LocalDateTime.MIN, LocalDateTime.MAX) :: Nil),
      List(
        (LocalDateTime.parse("2021-01-01T00:00:00"), Duration.parse("PT2H")),
        (LocalDateTime.parse("2021-01-02T00:00:00"), Duration.parse("PT5H"))
      )
    )
  }

  test("levelの挙動テスト") {
    val ldt = LocalDateTime.of(2021, 1, 1, 0, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt), // 1/1 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8)), // 1/1 08:00
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(1)), // 1/2 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8).plusDays(1)), // 1/2 08:00
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(2)), // 1/3 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8).plusDays(2)) // 1/3 08:00
      )
    Nil
    val upperTimes = List(
      UpperTimes(1, 1, 1, LocalTime.of(7, 0, 0), LocalDateTime.MIN, LocalDateTime.MIN)
    )
    assertEquals(
      level(bList(default(actions, Nil)).sortBy(_._1), upperTimes, Nil),
      List(
        (LocalDateTime.parse("2021-01-01T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-02T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-03T00:00:00"), Duration.parse("PT10H"))
      )
    )
  }

  test("levelの挙動テスト") {
    val ldt = LocalDateTime.of(2021, 1, 1, 0, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt), // 1/1 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(15)), // 1/1 15:00 15H
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(1)), // 1/2 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(10).plusDays(1)), // 1/2 10:00 10H
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(2)), // 1/3 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8).plusDays(2)) // 1/3 08:00 8H
      )
    Nil
    val upperTimes = List(
      UpperTimes(1, 1, 1, LocalTime.of(7, 0, 0), LocalDateTime.MIN, LocalDateTime.MIN)
    )
    assertEquals(
      level(bList(default(actions, Nil)).sortBy(_._1), upperTimes, Nil),
      List(
        (LocalDateTime.parse("2021-01-01T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-02T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-03T00:00:00"), Duration.parse("PT19H"))
      )
    )
  }

  test("levelの挙動テスト") {
    val ldt = LocalDateTime.of(2021, 1, 1, 0, 0, 0, 0)
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, ldt), // 1/1 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(10)), // 1/1 10:00 10H
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(1)), // 1/2 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(8).plusDays(1)), // 1/2 08:00 8H
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(2)), // 1/3 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(7).plusDays(2)), // 1/3 07:00 7H
        Actions(1, 1, 1, EventAction.Start, ldt.plusDays(3)), // 1/4 00:00
        Actions(1, 1, 1, EventAction.Finish, ldt.plusHours(1).plusDays(3)) // 1/4 01:00 1H
      )
    Nil
    val upperTimes = List(
      UpperTimes(1, 1, 1, LocalTime.of(7, 0, 0), LocalDateTime.MIN, LocalDateTime.MIN)
    )
    assertEquals(
      level(bList(default(actions, Nil)).sortBy(_._1), upperTimes, Nil),
      List(
        (LocalDateTime.parse("2021-01-01T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-02T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-03T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2021-01-04T00:00:00"), Duration.parse("PT5H"))
      )
    )
  }

  // 通話時のメモ 数日で通話で確認してこのコメントを削除したい
  // フォワードを適用してた時に同じアルゴリズムの重複になる
  // フォワード&&7時間上限=>7溜まらないシチュエーションが発生する(早退など)
  // 仮にフォワード適用した22start && 7h上限も設けたい=>フォワードを優先するのか7h優先するのかのコンフリクト
  test(
    "稼働があって7hに満たなかった日(12/03)は満たなかった日(12/03)の稼働が7hとなるまで翌日(12/04)の稼働分を奪う。奪われた日(12/04)の稼働時間が0になった場合は12/05から奪わない。".ignore
  ) {
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, LocalDateTime.parse("2007-12-03T23:00:00")),
        Actions(1, 1, 1, EventAction.Finish, LocalDateTime.parse("2007-12-04T01:00:00")), //2H
        Actions(1, 1, 1, EventAction.Start, LocalDateTime.parse("2007-12-04T10:00:00")),
        Actions(1, 1, 1, EventAction.Finish, LocalDateTime.parse("2007-12-04T11:00:00")), //1H
        Actions(1, 1, 1, EventAction.Start, LocalDateTime.parse("2007-12-0510:00:00")),
        Actions(1, 1, 1, EventAction.Finish, LocalDateTime.parse("2007-12-05T11:00:00")) //1H
      )

    val borderTimes = None
    val upperTimes = List(
      UpperTimes(
        1,
        1,
        1,
        LocalTime.parse("07:00:00"),
        LocalDateTime.MIN, //createdAt
        LocalDateTime.MIN //from
      )
    )
    assertEquals(
      applyAlgorithm(actions, borderTimes, upperTimes, GetAdvance, (LocalDateTime.MIN, LocalDateTime.MAX) :: Nil),
      List(
        (LocalDateTime.parse("2007-12-03T00:00:00"), Duration.parse("PT3H")),
        (LocalDateTime.parse("2007-12-04T00:00:00"), Duration.parse("PT0H")),
        (LocalDateTime.parse("2007-12-05T00:00:00"), Duration.parse("PT1H"))
      )
    )
  }

  test("稼働があって7hに満たなかった日は満たなかった日の稼働が7hとなるまで翌日の稼働分を奪う".ignore) {
    val actions =
      List(
        Actions(1, 1, 1, EventAction.Start, LocalDateTime.parse("2007-12-01T23:00:00")),
        Actions(1, 1, 1, EventAction.Finish, LocalDateTime.parse("2007-12-02T07:00:00")) //8H
      )
    val borderTimes = None
    val upperTimes = List(
      UpperTimes(
        1,
        1,
        1,
        LocalTime.parse("07:00:00"),
        LocalDateTime.MIN, //createdAt
        LocalDateTime.MIN //from
      )
    )
    assertEquals(
      applyAlgorithm(actions, borderTimes, upperTimes, GetAdvance, (LocalDateTime.MIN, LocalDateTime.MAX) :: Nil),
      List(
        (LocalDateTime.parse("2007-12-01T00:00:00"), Duration.parse("PT7H")),
        (LocalDateTime.parse("2007-12-02T00:00:00"), Duration.parse("PT1H"))
      )
    )
  }
}
