package com.softwaremill.bootzooka.timecardsystems

import cats.implicits.catsSyntaxOptionId
import com.softwaremill.bootzooka.timecardsystems.CoreOperation.{lastPositions, subtotals, totalTimeOfOneProject}

import java.time.{Duration, LocalDateTime}
import scala.annotation.tailrec

object applyAlgorithmSupports {

  /**
    * ↓会議メモ
    * 処理が1日以上開くとborderTimesが時間分秒しか持てないからaListSupportを作った。空白の操作をborderで一回ぶった斬るためのaListSupport。
    * 空白の操作：休憩開始してから3日経ったborderTimesで一回finishとかを補助的に内部的に挟んでいくやつ。
    * @param actions
    * @param memory
    * @param borderTimes Nilだったらborderが無いから
    * @return
    */
  def aList(
    actions: List[Actions],
    memory: List[(LocalDateTime, Actions)],
    borderTimes: List[BorderTimes]
  ): List[(LocalDateTime, Actions)] =
    actions match {
      case Nil => Nil
      case _ =>
        val slidedActions = actions.sliding(2).toList
        val remaining = actions.drop(1)
        val resultOpt = for {
          period <- slidedActions.headOption
          periodHead <- period.headOption
          periodLast <- period.lastOption
        } yield {
          val timeDiff = Duration.between(periodHead.createdAt, periodLast.createdAt)
          val isExtendDay = periodHead.createdAt.getYear != periodLast.createdAt.getYear ||
            periodHead.createdAt.getMonthValue != periodLast.createdAt.getMonthValue ||
            periodHead.createdAt.getDayOfMonth != periodLast.createdAt.getDayOfMonth
          val borderOpt = borderTimes.span(n => periodHead.createdAt.isAfter(n.appliedAt))._1.lastOption
          borderOpt.fold(
            memory :::
              (
                LocalDateTime
                  .of(
                    periodHead.createdAt.getYear,
                    periodHead.createdAt.getMonthValue,
                    periodHead.createdAt.getDayOfMonth,
                    0,
                    0,
                    0
                  ).plusDays(0),
                periodHead
              ) :: Nil
          ) { border =>
            period match {
              case _
                  if (periodHead.createdAt.toLocalTime.isBefore(border.borderTime) &&
                    border.borderTime.isBefore(periodLast.createdAt.toLocalTime)) ||
                    ((border.borderTime.isBefore(periodLast.createdAt.toLocalTime) &&
                      periodLast.createdAt.toLocalTime.isBefore(periodHead.createdAt.toLocalTime)) &&
                      isExtendDay) ||
                    ((periodHead.createdAt.toLocalTime.isBefore(border.borderTime) &&
                      periodLast.createdAt.toLocalTime.isBefore(periodHead.createdAt.toLocalTime)) &&
                      isExtendDay) &&
                    timeDiff.getSeconds < 86400 => //２間で日付が変わっていて差が１日未満ならば
                if (
                  isExtendDay && border.borderTime.isBefore(periodLast.createdAt.toLocalTime) &&
                  periodLast.createdAt.toLocalTime.isBefore(periodHead.createdAt.toLocalTime)
                ) period match {
                  case _ if periodHead.eventAction == EventAction.RestStart =>
                    memory :::
                      localDateTimeFromActions(periodHead, periodHead, 0) ::
                      (
                        LocalDateTime
                          .of(
                            periodHead.createdAt.getYear,
                            periodHead.createdAt.getMonthValue,
                            periodHead.createdAt.getDayOfMonth,
                            0,
                            0,
                            0
                          ).plusDays(0),
                        Actions(
                          periodHead.id,
                          periodHead.userId,
                          periodHead.companyId,
                          EventAction.Finish,
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(1)
                        )
                      ) :: (
                        LocalDateTime
                          .of(
                            periodHead.createdAt.getYear,
                            periodHead.createdAt.getMonthValue,
                            periodHead.createdAt.getDayOfMonth,
                            0,
                            0,
                            0
                          ).plusDays(1),
                        Actions(
                          periodHead.id,
                          periodHead.userId,
                          periodHead.companyId,
                          EventAction.Start,
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(1)
                        )
                      ) :: (
                        LocalDateTime
                          .of(
                            periodHead.createdAt.getYear,
                            periodHead.createdAt.getMonthValue,
                            periodHead.createdAt.getDayOfMonth,
                            0,
                            0,
                            0
                          ).plusDays(1),
                        Actions(
                          periodHead.id,
                          periodHead.userId,
                          periodHead.companyId,
                          EventAction.RestStart,
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(1)
                        )
                      ) :: Nil
                  case _ if periodHead.eventAction == EventAction.Finish =>
                    memory :::
                      localDateTimeFromActions(periodHead, periodHead, 0) :: Nil
                  case _ =>
                    memory :::
                      localDateTimeFromActions(periodHead, periodHead, 0) ::
                      (
                        LocalDateTime.of(
                          periodHead.createdAt.getYear,
                          periodHead.createdAt.getMonthValue,
                          periodHead.createdAt.getDayOfMonth,
                          0,
                          0,
                          0
                        ),
                        Actions(
                          periodHead.id,
                          periodHead.userId,
                          periodHead.companyId,
                          EventAction.Finish,
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(1)
                        )
                      ) :: (
                        LocalDateTime
                          .of(
                            periodHead.createdAt.getYear,
                            periodHead.createdAt.getMonthValue,
                            periodHead.createdAt.getDayOfMonth,
                            0,
                            0,
                            0
                          ).plusDays(1),
                        Actions(
                          periodHead.id,
                          periodHead.userId,
                          periodHead.companyId,
                          EventAction.Start,
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(1)
                        )
                      ) :: Nil
                }
                else
                  period match {
                    case _ if periodHead.eventAction == EventAction.RestStart =>
                      memory :::
                        localDateTimeFromActions(periodHead, periodHead, 0) ::
                        (
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              0,
                              0,
                              0
                            ).plusDays(0),
                          Actions(
                            periodHead.id,
                            periodHead.userId,
                            periodHead.companyId,
                            EventAction.Finish,
                            LocalDateTime
                              .of(
                                periodHead.createdAt.getYear,
                                periodHead.createdAt.getMonthValue,
                                periodHead.createdAt.getDayOfMonth,
                                border.borderTime.getHour,
                                border.borderTime.getMinute,
                                border.borderTime.getSecond
                              ).plusDays(0)
                          )
                        ) :: (
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              0,
                              0,
                              0
                            ).plusDays(1),
                          Actions(
                            periodHead.id,
                            periodHead.userId,
                            periodHead.companyId,
                            EventAction.Start,
                            LocalDateTime.of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            )
                          )
                        ) :: (
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              0,
                              0,
                              0
                            ).plusDays(1),
                          Actions(
                            periodHead.id,
                            periodHead.userId,
                            periodHead.companyId,
                            EventAction.RestStart,
                            LocalDateTime.of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            )
                          )
                        ) :: Nil
                    case _ if periodHead.eventAction == EventAction.Finish =>
                      memory :::
                        localDateTimeFromActions(periodHead, periodHead, 0) :: Nil
                    case _ =>
                      memory :::
                        localDateTimeFromActions(periodHead, periodHead, 0) ::
                        (
                          LocalDateTime.of(
                            periodHead.createdAt.getYear,
                            periodHead.createdAt.getMonthValue,
                            periodHead.createdAt.getDayOfMonth,
                            0,
                            0,
                            0
                          ),
                          Actions(
                            periodHead.id,
                            periodHead.userId,
                            periodHead.companyId,
                            EventAction.Finish,
                            LocalDateTime.of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            )
                          )
                        ) :: (
                          LocalDateTime
                            .of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              0,
                              0,
                              0
                            ).plusDays(1),
                          Actions(
                            periodHead.id,
                            periodHead.userId,
                            periodHead.companyId,
                            EventAction.Start,
                            LocalDateTime.of(
                              periodHead.createdAt.getYear,
                              periodHead.createdAt.getMonthValue,
                              periodHead.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            )
                          )
                        ) :: Nil
                  }

              case _ if periodHead.createdAt.toLocalTime.isAfter(border.borderTime) =>
                memory :::
                  localDateTimeFromActions(periodHead, periodHead, 0) :: Nil

              case _ =>
                memory :::
                  localDateTimeFromActions(periodHead, periodHead, 0) :: Nil
            }
          }
        }
        resultOpt.fold[List[(LocalDateTime, Actions)]](Nil) { result =>
          remaining match {
            case Nil => result
            case nel => aList(nel, result, borderTimes)
          }
        }
    }

  def bList(algList: List[(LocalDateTime, Actions)]): List[(LocalDateTime, Duration)] = {
    val keys = algList.groupBy(_._1).keys.toList
    for (i <- keys) yield {
      val target = algList.filter(_._1 == i).map(_._2)
      val (startPos, finishPos, _, _) = lastPositions(target)
      (
        i,
        totalTimeOfOneProject(
          subtotals(target, startPos, finishPos).map(n => n.fold(_ => Duration.ZERO, identity))
        )
      )
    }
  }

  @tailrec def aListSupport(
    al: List[(LocalDateTime, Actions)],
    memory: List[(LocalDateTime, Actions)],
    borderTimes: List[BorderTimes]
  ): List[(LocalDateTime, Actions)] =
    al match {
      case Nil => Nil
      case _ =>
        val slidedAList = al.sliding(2).toList
        val remaining = al.drop(1)
        val resultOpt = for {
          period <- slidedAList.headOption
          periodHead <- period.headOption
          periodLast <- period.lastOption
        } yield {
          val durationSec = Duration.between(periodHead._2.createdAt, periodLast._2.createdAt).getSeconds
          val borderOpt = borderTimes.span(n => periodHead._2.createdAt.isAfter(n.appliedAt))._1.lastOption
          borderOpt.fold(periodHead :: Nil)(border =>
            if (
              durationSec >= 86400 &&
              !(periodHead._2.eventAction == EventAction.Finish &&
                periodLast._2.eventAction == EventAction.Start)
            )
              (0 until (durationSec / 86400).toInt)
                .map { i =>
                  periodHead._2.eventAction match {
                    case EventAction.RestStart =>
                      (
                        periodHead._1.plusDays(i),
                        Actions(
                          periodHead._2.id,
                          periodHead._2.userId,
                          periodHead._2.companyId,
                          EventAction.Finish,
                          LocalDateTime
                            .of(
                              periodHead._2.createdAt.getYear,
                              periodHead._2.createdAt.getMonthValue,
                              periodHead._2.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(i + 1)
                        )
                      ) :: (
                        periodHead._1.plusDays(i + 1),
                        Actions(
                          periodHead._2.id,
                          periodHead._2.userId,
                          periodHead._2.companyId,
                          EventAction.Start,
                          LocalDateTime
                            .of(
                              periodHead._2.createdAt.getYear,
                              periodHead._2.createdAt.getMonthValue,
                              periodHead._2.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(i + 1)
                        )
                      ) :: (
                        periodHead._1.plusDays(i + 1),
                        Actions(
                          periodHead._2.id,
                          periodHead._2.userId,
                          periodHead._2.companyId,
                          EventAction.RestStart,
                          LocalDateTime
                            .of(
                              periodHead._2.createdAt.getYear,
                              periodHead._2.createdAt.getMonthValue,
                              periodHead._2.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(i + 1)
                        )
                      ) :: Nil
                    case _ =>
                      (
                        periodHead._1.plusDays(i),
                        Actions(
                          periodHead._2.id,
                          periodHead._2.userId,
                          periodHead._2.companyId,
                          EventAction.Finish,
                          LocalDateTime
                            .of(
                              periodHead._2.createdAt.getYear,
                              periodHead._2.createdAt.getMonthValue,
                              periodHead._2.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(i)
                        )
                      ) :: (
                        periodHead._1.plusDays(i + 1),
                        Actions(
                          periodHead._2.id,
                          periodHead._2.userId,
                          periodHead._2.companyId,
                          EventAction.Start,
                          LocalDateTime
                            .of(
                              periodHead._2.createdAt.getYear,
                              periodHead._2.createdAt.getMonthValue,
                              periodHead._2.createdAt.getDayOfMonth,
                              border.borderTime.getHour,
                              border.borderTime.getMinute,
                              border.borderTime.getSecond
                            ).plusDays(i)
                        )
                      ) :: Nil
                  }
                }.toList.flatten
            else Nil
          )
        }
        resultOpt match {
          case None => Nil
          case Some(result) =>
            al.headOption match {
              case None => Nil
              case Some(head) =>
                remaining match {
                  case Nil => memory ::: head :: result
                  case nel => aListSupport(nel, memory ::: head :: result, borderTimes)
                }
            }
        }
    }

  def localDateTimeFromActions(a: Actions, b: Actions, plusDay: Int) = (
    LocalDateTime
      .of(
        a.createdAt.getYear,
        a.createdAt.getMonthValue,
        a.createdAt.getDayOfMonth,
        0,
        0,
        0
      ).plusDays(plusDay),
    b
  )

  @tailrec def default(
    actions: List[Actions],
    memory: List[(LocalDateTime, Actions)]
  ): List[(LocalDateTime, Actions)] = actions match {
    case Nil => Nil
    case _ =>
      val wildResult = for {
        period <- actions.sliding(2).toList.headOption
        firstElement <- period.headOption
        secondElement <- period.drop(1).headOption
      } yield memory match {
        case Nil =>
          List(
            (firstElement, firstElement, 0),
            if (secondElement.eventAction == EventAction.Start) (secondElement, secondElement, 0)
            else (firstElement, secondElement, 0)
          ).map((localDateTimeFromActions _).tupled).some
        case _ =>
          for (memoryLast <- memory.lastOption)
            yield
              if (secondElement.eventAction == EventAction.Start) {
                List(
                  (secondElement, secondElement, 0)
                ).map((localDateTimeFromActions _).tupled)
              } else
                (
                  LocalDateTime.of(
                    memoryLast._1.getYear,
                    memoryLast._1.getMonthValue,
                    memoryLast._1.getDayOfMonth,
                    0,
                    0,
                    0
                  ),
                  secondElement
                ) :: Nil
      }
      val result =
        wildResult.fold[List[(LocalDateTime, Actions)]](memory)(
          _.fold[List[(LocalDateTime, Actions)]](memory)(memory ::: _)
        )
      actions.drop(1) match {
        case Nil       => result
        case remaining => default(remaining, result)
      }
  }

  @tailrec def level(
    target: List[(LocalDateTime, Duration)],
    upperTimes: List[UpperTimes],
    memory: List[(LocalDateTime, Duration)]
  ): List[(LocalDateTime, Duration)] =
    target match {
      case Nil        => memory
      case one :: Nil => memory ::: one :: Nil
      case nel =>
        val resultOpt = for {
          periodHead <- nel.take(2).headOption
          periodLast <- nel.take(2).lastOption
        } yield {
          val upperOpt = upperTimes.span(_.appliedAt isBefore periodHead._1)._1.lastOption
          upperOpt.fold[(List[(LocalDateTime, Duration)], (LocalDateTime, Duration))](
            memory ::: periodHead :: Nil,
            periodLast
          ) { upper =>
            val diff = periodHead._2.toSeconds - upper.upperTime.toSecondOfDay
            if (diff < 0) (memory ::: periodHead :: Nil, periodLast)
            else
              (
                memory :::
                  (
                    periodHead._1,
                    Duration.ofSeconds(upper.upperTime.toSecondOfDay)
                  ) :: Nil,
                (periodLast._1, periodLast._2.plusSeconds(diff))
              )
          }
        }
        val (result, remaining) = resultOpt match {
          case None    => (memory ::: nel, Nil)
          case Some(n) => (n._1, n._2 :: nel.drop(2))
        }
        remaining match {
          case Nil => result
          case nel => level(nel, upperTimes, result)
        }
    }

  def getAdvance(
    target: List[(LocalDateTime, Duration)],
    upperTimes: List[UpperTimes],
    memory: List[(LocalDateTime, Duration)]
  ) =
    target match {
      case Nil => Nil
      case nel =>
        val result = for {
          period <- nel.sliding(2).toList.headOption
          periodHead <- period.headOption
          periodLast <- period.lastOption
        } yield {
          val upperTimeOpt = upperTimes.span(_.appliedAt isBefore periodHead._1)._1.lastOption
          upperTimeOpt.fold[List[(LocalDateTime, Duration)]](memory ::: periodHead :: Nil) { n =>
            val diff = periodHead._2.toSeconds - n.upperTime.getSecond
            if (diff >= 0) memory ::: periodHead :: Nil
            else {
//              val alteredPeriodHead =
//                if (Math.abs(diff) > periodLast._2.toSeconds) (periodHead._1, periodHead._2.plus(periodLast._2))
              Nil
            }
          }
        }
    }
}
