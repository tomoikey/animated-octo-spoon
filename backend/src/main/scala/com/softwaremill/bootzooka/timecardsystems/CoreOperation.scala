package com.softwaremill.bootzooka.timecardsystems

import cats.data.NonEmptyList
import cats.implicits._
import com.markatta.timeforscala._
import com.typesafe.scalalogging.StrictLogging

import java.time.{Duration, LocalDateTime, ZoneOffset}
import scala.annotation.tailrec
import com.softwaremill.bootzooka.timecardsystems.applyAlgorithmSupports._

object CoreOperation extends StrictLogging {

  def firstPositions(actions: List[Actions]) = {
    val eventActions = actions.map(_.eventAction)
    val firstStartPos = eventActions.indexWhere(_ == EventAction.Start)
    val firstFinishPos = eventActions.indexWhere(_ == EventAction.Finish)
    val firstRestStartPos = eventActions.indexWhere(_ == EventAction.RestStart)
    val firstRestFinishPos = eventActions.indexWhere(_ == EventAction.RestFinish)
    (firstStartPos, firstFinishPos, firstRestStartPos, firstRestFinishPos)
  }

  def lastPositions(userStateInfoResult: List[Actions]) = {
    val eventActions = userStateInfoResult.map(_.eventAction)
    val lastStartPos = eventActions.lastIndexOf(EventAction.Start)
    val lastFinishPos = eventActions.lastIndexOf(EventAction.Finish)
    val lastRSPos = eventActions.lastIndexOf(EventAction.RestStart)
    val lastRFPos = eventActions.lastIndexOf(EventAction.RestFinish)
    (lastStartPos, lastFinishPos, lastRSPos, lastRFPos)
  }

  def identifyMyStateAt(actions: List[Actions]) = {
    val (lSP, lFP, lSRP, lFRP) = lastPositions(actions)
    if ((lFP > lFRP && lFP > lSRP && lFP > lSP) || (lFP == lSP && lSP == lSRP && lSRP == lFP)) State.何もしていません.toString
    else if (lSRP > lFRP && lSRP > lFP) State.休憩中.toString
    else State.勤務中.toString
  }

  def showDuration(duration: Duration): String =
    (duration.toHoursPart.formatted("%02d") ::
      duration.toMinutesPart.formatted("%02d") ::
      duration.toSecondsPart.formatted("%02d") ::
      duration.toMillisPart.formatted("%03d") :: Nil).mkString(":")

  /**
    * @param actions ソート済み先頭が古いcreatedAtの配列を想定している。ユーザーの内容変更はソートしてから入れてください。
    * @return
    */
  def subtotals(actions: List[Actions], lastStart: Int, lastFinish: Int): List[Either[Error, Duration]] =
    if (actions.length <= 1 || actions.length - lastStart == 1) Nil
    else {
      val target =
        if (actions.exists(_.eventAction == EventAction.Finish) && lastStart < lastFinish)
          actions.slice(lastStart, lastFinish + 1)
        else actions.slice(lastStart, actions.length)
      target.sliding(2).toList.map {
        case Nil                                                       => Error.UnknownException.asLeft
        case _ :: Nil                                                  => Error.UnknownException.asLeft
        case head :: last :: _ if head.eventAction == last.eventAction => Error.ContinualSameValueException.asLeft
        case head :: last :: _
          if head.eventAction == EventAction.RestStart && last.eventAction == EventAction.RestFinish =>
          Duration.ZERO.asRight
        case head :: last :: _ if head.eventAction == EventAction.RestStart && last.eventAction == EventAction.Finish =>
          Duration.ZERO.asRight
        case head :: last :: _ if head.eventAction == EventAction.Start && last.eventAction == EventAction.RestFinish =>
          Error.NoStartRestException.asLeft
        case head :: last :: _ if head.eventAction == EventAction.Start =>
          Duration.between(head.createdAt, last.createdAt).asRight
        case head :: last :: _
          if head.eventAction == EventAction.RestFinish && last.eventAction == EventAction.RestStart =>
          Duration.between(head.createdAt, last.createdAt).asRight
        case head :: last :: _
          if head.eventAction == EventAction.RestFinish && last.eventAction == EventAction.Finish =>
          Duration.between(head.createdAt, last.createdAt).asRight
        case _ => Error.UnknownException.asLeft
      }
    }

  /**
    * @param actions ソート済み先頭が古いcreatedAtの配列を想定している
    * @return
    */
  def listTotalTimes(actions: List[Actions]): (List[Either[Error, Duration]], Duration) = {
    @tailrec def f(
                    actions: List[Actions],
                    memory: (List[Either[Error, Duration]], Duration)
                  ): (List[Either[Error, Duration]], Duration) = {
      val (subPeriod, subRemaining) = actions.span(_.eventAction != EventAction.Finish)
      val (period, remaining) = (subRemaining.headOption.fold(subPeriod)(subPeriod ::: _ :: Nil), subRemaining.drop(1))
      val (startPos, finishPos, _, _) = firstPositions(period)
      val allSubtotal =
        if (!period.map(_.eventAction).contains(EventAction.Start))
          memory._1 :::
            Error.FirstElementException.asLeft ::
            subtotals(period, startPos, finishPos)
        else
          memory._1 :::
            subtotals(period, startPos, finishPos)

      val periodDurations = subtotals(period, startPos, finishPos).map(_.fold(_ => Duration.ZERO, identity))
      val currentAllTotalTime = totalTimeOfOneProject(memory._2 :: totalTimeOfOneProject(periodDurations) :: Nil)
      val result = (allSubtotal, currentAllTotalTime)
      remaining match {
        case Nil                                                     => result
        case nel if !nel.exists(_.eventAction == EventAction.Finish) => result
        case nel                                                     => f(nel, result)
      }
    }
    actions match {
      case Nil => (Nil, Duration.ZERO)
      case nel => f(nel, (Nil, Duration.ZERO))
    }
  }

  def applyAlgorithm(
                      actions: List[Actions],
                      borderTimes: Option[NonEmptyList[BorderTimes]],
                      upperTimes: List[UpperTimes],
                      algorithm: UserAlgorithm,
                      applySection: List[(LocalDateTime, LocalDateTime)]
                    ): List[(LocalDateTime, Duration)] =
    algorithm match {
      case UserAlgorithm.Default =>
        bList(default(actions, Nil))
      case UserAlgorithm.Forward =>
        bList(
          aListSupport(
            aList(actions, Nil, borderTimes.toList.flatMap(_.toList)),
            Nil,
            borderTimes.toList.flatMap(_.toList)
          )
        ).sortBy(_._1)
      case UserAlgorithm.GetAdvance =>
        Nil
      case UserAlgorithm.Level =>
        Nil
    }

  /**
    * @param actions 一つの企業におけるactionsを入れてください
    * @param span 範囲指定方法（以上、未満）
    * @return
    */
  def takeTotalTimeFromFreeSpan(
                                 actions: List[Actions],
                                 span: (LocalDateTime, LocalDateTime)
                               ): (List[Either[Error, Duration]], Duration) = {
    val f: List[Actions] => (List[Either[Error, Duration]], Duration) = (a: List[Actions]) =>
      a.headOption.fold[(List[Either[Error, Duration]], Duration)]((Nil, Duration.ZERO))(head =>
        if (head.eventAction != EventAction.Start)
          listTotalTimes(Actions(0, head.userId, head.companyId, EventAction.Start, head.createdAt) :: a)
        else listTotalTimes(a)
      )
    val (_, subPeriod) =
      actions.span(_.createdAt.toInstant(ZoneOffset.UTC) < span._1.toInstant(ZoneOffset.UTC))
    val (period, _) = subPeriod.span(
      _.createdAt.toInstant(ZoneOffset.UTC) < span._2.toInstant(ZoneOffset.UTC)
    )
    period.headOption.fold[(List[Either[Error, Duration]], Duration)]((Nil, Duration.ZERO))(_ => f(period))
  }

  def isExistSameValue[T](list: List[T]) = list match {
    case Nil | _ :: Nil => false
    case l =>
      l.sliding(2).toList.exists { n =>
        n match {
          case Nil | _ :: Nil      => false
          case head :: last :: Nil => head == last
          case _                   => false
        }
      }
  }

  def toEpochSec(ldt: LocalDateTime): Long = ldt.toInstant(ZoneOffset.ofHours(9)).toEpochMilli

  def totalTimeOfOneProject(target: List[Duration]): Duration = target.foldLeft[Duration](Duration.ZERO)(_ + _)
}

