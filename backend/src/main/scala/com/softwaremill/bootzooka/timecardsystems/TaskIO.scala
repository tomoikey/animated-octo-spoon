package com.softwaremill.bootzooka.timecardsystems

import cats.effect.IO
import doobie.{LogHandler, Transactor}
import doobie.implicits._
import doobie.implicits.javatime._
import com.softwaremill.bootzooka.timecardsystems.CoreOperation.{identifyMyStateAt, lastPositions, subtotals, totalTimeOfOneProject}

import java.time.{Duration, LocalDateTime}
import scala.concurrent.duration.{Duration => _, _}

object TaskIO {

  def takeFromActions(xa: Transactor[IO]): IO[List[Actions]] =
    sql"select* from actions".queryWithLogHandler[Actions](LogHandler.jdkLogHandler).to[List].transact(xa)

  def takeFromCompany(xa: Transactor[IO]): IO[List[Companies]] =
    sql"select* from company".queryWithLogHandler[Companies](LogHandler.jdkLogHandler).to[List].transact(xa)

  def takeFromUsers(xa: Transactor[IO]): IO[List[Users]] =
    sql"select* from users".queryWithLogHandler[Users](LogHandler.jdkLogHandler).to[List].transact(xa)

  def updateActionsTable(
    xa: Transactor[IO],
    userId: Long,
    companyId: Long,
    eventAction: EventAction,
    createdAt: LocalDateTime
  ) =
    sql"insert into actions (user_id, company_id, event_action, created_at) values ($userId, $companyId, $eventAction, $createdAt)".update.run
      .transact(xa).unsafeRunTimed(30.seconds)

  def whereIWorkOn(xa: Transactor[IO], userId: Long) =
    sql"select* from company where user_id=$userId"
      .queryWithLogHandler[Companies](LogHandler.jdkLogHandler).to[List].transact(xa).unsafeRunTimed(30.seconds).get

  def listStatusByRegisteredCompany(xa: Transactor[IO], userId: Long) =
    sql"select* from company where user_id=$userId"
      .queryWithLogHandler[Companies](LogHandler.jdkLogHandler)
      .to[List].transact(xa).unsafeRunTimed(30.seconds).get
      .map(n => identifyMyStateAt(selectFromCompany(xa, n.id, userId)))

  def selectFromCompany(xa: Transactor[IO], companyId: Long, userId: Long) =
    sql"select* from actions where user_id=$userId and company_id=$companyId"
      .query[Actions].to[List].transact(xa).unsafeRunTimed(30.seconds).get

  def eachTimerStatus(xa: Transactor[IO], userId: Long) = {
    def timer(companyId: Long) = {
      val actions = selectFromCompany(xa, companyId, userId)
      val state = identifyMyStateAt(actions)
      val createdAts = selectFromCompany(xa, companyId, userId).map(_.createdAt)
      val (lSP, _, _, _) = lastPositions(actions)
      if (state == State.勤務中.toString || state == State.休憩中.toString) createdAts(lSP).toString
      else
        State.NotInWorking.toString
    }
    sql"select id from company where user_id=$userId"
      .queryWithLogHandler[Long](LogHandler.jdkLogHandler)
      .to[List].transact(xa).unsafeRunTimed(30.seconds).get.map(timer)
  }

  def loginAs(
    xa: Transactor[IO],
    allCompanies: List[Companies],
    userId: Long
  ): (
    List[Companies],
    List[String],
    List[String],
    List[(List[String], Duration)]
  ) = {
    val myCompanies = allCompanies.filter(_.userId == userId)
    val myStatuses = listStatusByRegisteredCompany(xa, userId)
    val TimeAboutMyProjects = eachTimerStatus(xa, userId)
    val subtotalsAboutMyCompanies = myCompanies.map(n => selectFromCompany(xa, n.id, userId)).map { n =>
      val lps = lastPositions(n)
      (
        subtotals(n, lps._1, lps._2).map(_.toString),
        totalTimeOfOneProject(subtotals(n, lps._1, lps._2).map(_.fold(_ => Duration.ZERO, identity)))
      )
    }

    (
      myCompanies,
      myStatuses,
      TimeAboutMyProjects,
      subtotalsAboutMyCompanies
    )
  }
}
