package com.softwaremill.bootzooka.timecardsystems

import java.time.LocalDateTime

case class Actions(id: Long, userId: Long, companyId: Long, eventAction: EventAction, createdAt: LocalDateTime)
