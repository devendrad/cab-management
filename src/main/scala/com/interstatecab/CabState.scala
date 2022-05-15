package com.interstatecab

object ValidCabState extends Enumeration {
  type ValidCabState = Value

  val IDLE = Value("IDLE")
  val ON_TRIP = Value("ON_TRIP")

  val IDLEs = IDLE.toString
  val ON_TRIPs = ON_TRIP.toString

}
