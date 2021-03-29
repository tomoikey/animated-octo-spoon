package com.softwaremill.bootzooka.timecardsystems

import enumeratum.EnumEntry.LowerCamelcase
import enumeratum._

sealed trait EventAction extends EnumEntry with LowerCamelcase

object EventAction extends PlayEnum[EventAction] with DoobieEnum[EventAction] {
  val values = findValues
  case object Start extends EventAction
  case object Finish extends EventAction
  case object RestStart extends EventAction
  case object RestFinish extends EventAction
}
