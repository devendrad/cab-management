package com.interstatecab

import ValidCabState.ValidCabState

case class Cab(id: String, var locationCity: City) {

  private var stateHistory = List.empty[CabHistoryEntry]

  var state: CabState = changeStateToIdle

  def changeLocationCity(newLocationCity: City) = {
    locationCity = newLocationCity
    this
  }

  def changeState(newCabState: ValidCabState): Cab = {
    newCabState match {
      case ValidCabState.IDLE => changeStateToIdle
      case ValidCabState.ON_TRIP => changeStateToOnTrip
    }
    this
  }

  private def changeStateToIdle: CabState = {
    val newState = IDLE(System.currentTimeMillis())
    captureHistory(newState)
    state = newState
    newState
  }

  private def changeStateToOnTrip: CabState = {
    val newState = ON_TRIP(System.currentTimeMillis())
    captureHistory(newState)
    state = newState
    newState
  }

  /**
    * time, state
    * 9999, IDLE
    * 10002, ON_TRIP
    * 10005, IDLE
    * 10010, ON_TRIP
    *
    * @param state
    */
  private def captureHistory(state: CabState): Unit = {
    stateHistory = stateHistory :+ CabHistoryEntry(state.time, state.toString)
  }

  def getHistory: List[CabHistoryEntry] = stateHistory.toList

  def calculateCabStateAggregateTime(startTimestampOpt: Option[Long] = None, endTimestampOpt: Option[Long] = None): CabInsights = {

    val startTimestamp = startTimestampOpt.getOrElse(0L)
    val endTimestamp = endTimestampOpt.getOrElse(System.currentTimeMillis())

    // Get normalized history
    val firstNormalizedRecord: Option[CabHistoryEntry] = stateHistory.filter(startTimestamp > _.timestamp).lastOption.map {
      record =>
        CabHistoryEntry(startTimestamp, record.state)
    }

    val lastNormalizedRecord: Option[CabHistoryEntry] = stateHistory.filter(endTimestamp > _.timestamp).lastOption.map {
      record =>
        CabHistoryEntry(endTimestamp, record.state)
    }

    val filteredHistory: List[CabHistoryEntry] =
      stateHistory.filter(record => startTimestamp <= record.timestamp && endTimestamp >= record.timestamp)

    val normalizedHistory: List[CabHistoryEntry] = List(firstNormalizedRecord).flatten ++ filteredHistory ++ lastNormalizedRecord

    val zeroethEntryTimestamp = normalizedHistory.headOption.map(_.timestamp).getOrElse(0L)
    val zeroethEntryState = normalizedHistory.headOption.map(_.state).getOrElse(ValidCabState.IDLEs)
    val (idleDuration, onTripDuration, _, _) = normalizedHistory.foldLeft((0L, 0L, zeroethEntryState, zeroethEntryTimestamp)) {
      (agg, singleHistoryRecord) =>

        val (idleTime, tripTime, previousState, previousEntryTimestamp) = (agg._1, agg._2, agg._3, agg._4)

        val calculatedDurationCurrentState = singleHistoryRecord.timestamp - previousEntryTimestamp
        previousState match {
          case ValidCabState.IDLEs => (idleTime + calculatedDurationCurrentState, tripTime, singleHistoryRecord.state, singleHistoryRecord.timestamp)
          case ValidCabState.ON_TRIPs => (idleTime, tripTime + calculatedDurationCurrentState, singleHistoryRecord.state, singleHistoryRecord.timestamp)
        }
    }

    CabInsights(idleDuration, onTripDuration, startTimestampOpt, endTimestampOpt)
  }
}




sealed trait CabState {
  def chageToOnTrip: CabState = this
  def chageToIdle: CabState = this // TODO
  def time: Long
}

case class IDLE(time: Long) extends CabState {
  override def chageToOnTrip(): CabState = ON_TRIP(System.currentTimeMillis())
  override val toString = ValidCabState.IDLEs
}

case class ON_TRIP(time: Long) extends CabState {
  override def chageToIdle: CabState = IDLE(System.currentTimeMillis())
  override val toString = ValidCabState.ON_TRIPs
}


