package com.softwaremill.bootzooka.timecardsystems

sealed trait Error

object Error {
  case object ContinualSameValueException extends Error
  case object DoubleStartException extends Error
  case object NoFinishException extends Error
  case object FirstElementException extends Error
  case object NoStartRestException extends Error
  case object Nothing extends Error
  case object UnknownException extends Error
}
