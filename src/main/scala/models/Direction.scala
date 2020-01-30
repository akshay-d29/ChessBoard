package models

sealed trait Direction

case object Horizontal extends Direction

case object Vertical extends Direction

case object Diagonal extends Direction
