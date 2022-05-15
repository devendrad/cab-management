package com.interstatecab

import org.slf4j.LoggerFactory

import scala.util.Try

class CabManagementService{

  import CabManagementService._

  val log = LoggerFactory.getLogger(this.getClass)

  def registerCab(registerCabRequest: RegisterCabRequest): CabResponse = {

    log.info(s"Got request $registerCabRequest")

    // Gettinng City from ID
    val city = getCityFromId(registerCabRequest.location_city_id)

    // Checking if the cab is already registered
    cabs.find(_.id == registerCabRequest.cab_id).map{
      cab =>
        throw BadRequestException(s"Cab with id ${registerCabRequest.cab_id} is already registered")
    }

    // Registering the cab
    val newCab = new Cab(registerCabRequest.cab_id, city)
    cabs = cabs :+ newCab
    newCab.toResponseFormat()
  }

  def getCabs = {
    log.info(s"Got request to get all cabs")
    cabs.map(_.toResponseFormat())
  }

  def getSingleCab(cabId: String) = {
    log.info(s"Got request to get cab $cabId")
    getCabFromId(cabId).toResponseFormat()
  }

  def onboardCity(cityOnboardingRequest: CityOnboardingRequest): CityOnboardingRequest = {
    log.info(s"Got request $cityOnboardingRequest")
    cities = cities ++ cityOnboardingRequest.cities
    cityOnboardingRequest
  }

  def getCities = {
    log.info(s"Got request to get all cities")
    cities
  }

  def changeCabCity(cabCityChangeRequest: CabCityChangeRequest): CabResponse = {
    log.info(s"Got request $cabCityChangeRequest")
    val newCabCity = getCityFromId(cabCityChangeRequest.new_location_city_id)
    val cab = getCabFromId(cabCityChangeRequest.cab_id)
    if(cab.locationCity.id == cabCityChangeRequest.new_location_city_id)
      throw BadRequestException(s"Cab with Id ${cabCityChangeRequest.cab_id} is already in ${cabCityChangeRequest.new_location_city_id} city")
    else
      cab.changeLocationCity(newCabCity).toResponseFormat()
  }

  def changeCabState(cabStateChangeRequest: CabStateChangeRequest): CabResponse = {
    log.info(s"Got request $cabStateChangeRequest")
    val newState = Try(ValidCabState.withName(cabStateChangeRequest.new_cab_state)).toOption.getOrElse(throw BadRequestException(s"Invalid cab state ${cabStateChangeRequest.new_cab_state}"))
    val cab = getCabFromId(cabStateChangeRequest.cab_id)
    if(cab.state.toString == cabStateChangeRequest.new_cab_state)
      throw BadRequestException(s"Cab with Id ${cabStateChangeRequest.cab_id} is already in ${cabStateChangeRequest.new_cab_state} state")
    else
      cab.changeState(newState).toResponseFormat()
  }

  /**
    * Store the city and time so as to calculate high demand cities and high demand times
    * @param cabBookRequest
    * @return
    */
  def bookCab(cabBookRequest: CabBookRequest): CabResponse = {

    log.info(s"Got request $cabBookRequest")

    if(cabBookRequest.to_city_id == cabBookRequest.from_city_id) throw BadRequestException(s"Intra state cab booking facility is currenlty not supported")

    // Validate cities
    getCityFromId(cabBookRequest.from_city_id) // throws exception if city is not on-boarded
    getCityFromId(cabBookRequest.to_city_id)   // throws exception if city is not on-boarded

    updateBookingStatistics(cabBookRequest)

    cabs
      .filter(_.locationCity.id == cabBookRequest.from_city_id)         // Filtering with city
      .filter(_.state.toString == ValidCabState.IDLEs)                  // FIltering Idle cabs
      .map(cab => (cab, cab.calculateCabStateAggregateTime().idle_time_millis))    // (cab. idleTime)
      .sortWith((a,b) => a._2 > b._2)                                   // Sorting cabs in desc order of most idle ones
      .headOption.map(_._1).map(_.changeState(ValidCabState.ON_TRIP))   // Booking the cab
      .getOrElse(throw BadRequestException(s"Sorry no cabs are available at the moment in city ${cabBookRequest.from_city_id}"))
      .toResponseFormat()                                               // Returning the response
  }

  def updateBookingStatistics(cabBookRequest: CabBookRequest) = {
    val currentBookingCount: Long = citywiseCabBookings.getOrElse(cabBookRequest.from_city_id, 0L)
    citywiseCabBookings = citywiseCabBookings + (cabBookRequest.from_city_id -> (currentBookingCount + 1L))
  }

  def getCabInsights(cabId: String, startTimestampOpt: Option[Long], endTimestampOpt: Option[Long]): CabInsights = {
    log.info(s"Got request to getCabInsights $cabId $startTimestampOpt $endTimestampOpt")
    getCabFromId(cabId).calculateCabStateAggregateTime(startTimestampOpt, endTimestampOpt)
  }

  def getCabInsightsHistory(cabId: String): List[CabHistoryEntry] = {
    log.info(s"Got request to getCabInsightsHistory $cabId")
    getCabFromId(cabId).getHistory
  }

  def getTopCities() = {
    log.info(s"Got request to getTopCities")
    citywiseCabBookings.take(5)
  }

  private def getCityFromId(city_id: String): City = {
    cities.filter(_.id == city_id).headOption
      .getOrElse(throw BadRequestException(s"Invalid city id $city_id"))
  }

  private def getCabFromId(cab_id: String): Cab = {
    cabs.filter(_.id == cab_id).headOption
      .getOrElse(throw BadRequestException(s"Invalid cab id $cab_id"))
  }

  implicit class CabResponseFormat(cab: Cab) {
    def toResponseFormat() = {
      CabResponse(cab.id, cab.locationCity.id, cab.state.toString)
    }
  }
}

object CabManagementService{
  var cabs = List.empty[Cab]
  var cities = List.empty[City]
  var citywiseCabBookings = Map.empty[String, Long]
}