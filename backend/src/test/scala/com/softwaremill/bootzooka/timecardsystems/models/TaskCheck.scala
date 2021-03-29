package com.softwaremill.bootzooka.timecardsystems.models

import com.softwaremill.bootzooka.timecardsystems.CoreOperation.listTotalTimes
import com.softwaremill.bootzooka.timecardsystems.{Actions, EventAction}
import enumeratum.scalacheck.arbEnumEntry
import munit.ScalaCheckSuite
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import java.time.LocalDateTime
import scala.util.Try

class TaskCheck extends ScalaCheckSuite {

  implicit val arbEventAction: Arbitrary[Actions] = Arbitrary(for {
    eventAction <- Arbitrary.arbitrary[EventAction]
    ldt <- Arbitrary.arbitrary[LocalDateTime]
    id <- Arbitrary.arbLong.arbitrary
    userId <- Arbitrary.arbLong.arbitrary
    companyId <- Arbitrary.arbLong.arbitrary
  } yield Actions(id, userId, companyId, eventAction, ldt))

  property("listTotalTimesが例外を吐かない") {
    forAll { as: List[Actions] =>
      Try(listTotalTimes(as.sortBy(_.createdAt))).fold(exception(_), _ => passed)
    }
  }

  // DBでは重複するcreatedAtの場合弾くようにするのでこのテストに価値はない
  property("重複するcreatedAtの場合でもlistTotalTimesが例外を吐かない") {
    forAll { as: List[Actions] =>
      val now = LocalDateTime.now()
      Try(listTotalTimes(as.map(_.copy(createdAt = now)))).fold(exception(_), _ => passed)
    }
  }
}
