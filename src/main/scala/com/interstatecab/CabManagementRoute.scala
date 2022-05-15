package com.interstatecab

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.server.directives.RouteDirectives.complete
import spray.json._

case class RegisterCabRequest(cab_id: String, location_city_id: String, state: String)
case class City(id: String, name: Option[String])
case class CityOnboardingRequest(cities: List[City])
case class CabCityChangeRequest(cab_id: String, new_location_city_id: String)
case class CabStateChangeRequest(cab_id: String, new_cab_state: String)

case class CabResponse(cab_id: String, location_city_id: String, state: String)
case class CabBookRequest(from_city_id: String, to_city_id: String)
case class CabInsights(idle_time_millis: Long, trip_time_millis: Long, start_timestamp: Option[Long], end_timestamp: Option[Long])
case class CabHistoryEntry(timestamp: Long, state: String)

trait CabManagementJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val registerCabRequestFormat = jsonFormat3(RegisterCabRequest)
  implicit val cityFormat = jsonFormat2(City)
  implicit val cityOnboardingRequestFormat = jsonFormat1(CityOnboardingRequest)
  implicit val cabCityChangeRequestFormat = jsonFormat2(CabCityChangeRequest)
  implicit val cabStateChangeRequestFormat = jsonFormat2(CabStateChangeRequest)
  implicit val cabBookRequestFormat = jsonFormat2(CabBookRequest)

  implicit val cabResponseFormat = jsonFormat3(CabResponse)
  implicit val errorResponseFormat = jsonFormat1(ErrorResponse)
  implicit val cabInsightsFormat = jsonFormat4(CabInsights)
  implicit val cabHistoryEntryFormat = jsonFormat2(CabHistoryEntry)
}

class CabManagementRoute(service: CabManagementService) extends CabManagementJsonSupport {

  val route: Route = pathPrefix("cab-management" / "v1") {
    onboardCities ~ getCities ~
      registerCab ~ getSingleCab ~ getCabs ~ changeCabCity ~ changeCabState ~ getCabInsights ~ getCabHistory ~ bookCab ~
      getTopCities
  }

  def registerCab = path("cab"){
    post {
      entity(as[RegisterCabRequest]) {
        request =>
          complete(StatusCodes.OK, service.registerCab(request))
      }
    }
  }

  def getCabs = path("cab"){
    get {
      complete(StatusCodes.OK, service.getCabs)
    }
  }

  def getSingleCab = path("cab" / Segment){
    cabId =>
    get {
      complete(StatusCodes.OK, service.getSingleCab(cabId))
    }
  }

  def onboardCities = path("city"){
    post {
      entity(as[CityOnboardingRequest]) {
        request =>
          complete(StatusCodes.OK, service.onboardCity(request))
      }
    }
  }

  def getCities = path("city"){
    get {
      complete(StatusCodes.OK, service.getCities)
    }
  }

  def changeCabCity = path("cab"){
    put {
      entity(as[CabCityChangeRequest]) {
        request =>
          complete(StatusCodes.OK, service.changeCabCity(request))
      }
    }
  }

  def changeCabState = path("cab"){
    put {
      entity(as[CabStateChangeRequest]) {
        request =>
          complete(StatusCodes.OK, service.changeCabState(request))
      }
    }
  }

  def getCabInsights = path("cab-insights" / Segment){
    cabId =>
      get {
        parameter('start_timestamp.as[Long].?, 'end_timestamp.as[Long].?) {
          (startTimestampOpt, endTimestampOpt) =>
            complete(StatusCodes.OK, service.getCabInsights(cabId, startTimestampOpt, endTimestampOpt))
        }
      }
  }

  def getCabHistory = path("cab-history" / Segment){
    cabId =>
      get {
        complete(StatusCodes.OK, service.getCabInsightsHistory(cabId))
      }
  }

  def bookCab = path("cab-booking"){
    post {
      entity(as[CabBookRequest]) {
        request =>
          complete(StatusCodes.OK, service.bookCab(request))
      }
    }
  }

  def getTopCities = path("cab-booking-top-cities"){
    get {
      complete(StatusCodes.OK, service.getTopCities())
    }
  }
}
