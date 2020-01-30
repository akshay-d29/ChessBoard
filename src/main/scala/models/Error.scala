package models

sealed trait Error

case object InvalidInput extends Error

case object InvalidChessPiece extends Error

case object InvalidPosition extends Error
