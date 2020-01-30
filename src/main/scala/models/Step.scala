package models

sealed trait Step

case object One extends Step

case object TwoAndHalf extends Step

case object Multiple extends Step
