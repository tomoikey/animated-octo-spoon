package com.softwaremill.bootzooka.timecardsystems
import com.markatta.timeforscala.LocalTime
import java.time.{LocalDateTime}

/**
  * appliedAtからborderTimeを日替わり時刻とする
  * @param id
  * @param userId
  * @param companyId
  * @param borderTime
  * @param createdAt イベントのDBの情報を作った時間。イベント作成のログの表示では使われるけどリプレイでは無視される。
  * @param appliedAt リプレイ時のソートのキーになる。そっから(appliedAt)22:00じめ。1/3(appliedAt)みたいな。22:30:02(appliedAt)から適用させるとかができる。適用開始日(appliedAt)。
  *                  イベントを適用する時間(appliedAt)
  */
case class BorderTimes(
  id: Long,
  userId: Long,
  companyId: Long,
  borderTime: LocalTime,
  createdAt: LocalDateTime,
  appliedAt: LocalDateTime
)
