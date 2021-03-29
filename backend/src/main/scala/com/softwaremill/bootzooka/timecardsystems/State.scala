package com.softwaremill.bootzooka.timecardsystems

import enumeratum.EnumEntry.LowerCamelcase
import enumeratum._

sealed trait State extends EnumEntry with LowerCamelcase

object State extends PlayEnum[State] with DoobieEnum[State] {
  val values = findValues
  case object 勤務中 extends State
  case object 休憩中 extends State
  case object 何もしていません extends State
  case object NotInWorking extends State
  case object システムエラーが発生しました extends State
}
