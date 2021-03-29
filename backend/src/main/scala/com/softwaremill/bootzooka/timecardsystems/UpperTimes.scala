package com.softwaremill.bootzooka.timecardsystems

import com.markatta.timeforscala.LocalTime

import java.time.LocalDateTime

case class UpperTimes(
  id: Long,
  userId: Long,
  companyId: Long,
  upperTime: LocalTime,
  createdAt: LocalDateTime,
  appliedAt: LocalDateTime
)
