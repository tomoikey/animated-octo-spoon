package com.softwaremill.bootzooka.timecardsystems

sealed trait UserAlgorithm

object UserAlgorithm {

  /**
    *  １日の締めの時間をリストで取れる。日替わりの時刻は適用開始時間を持ってるから適用開始時間をすぎた以降のやつはそれが適用される。
    *  例えば1/1-1/3は22borderで、そっから先の日は21borderにしたいと思ったらそれができる
    */
  case object Forward extends UserAlgorithm

  case object GetAdvance extends UserAlgorithm
  case object Level extends UserAlgorithm
  case object Default extends UserAlgorithm
}
