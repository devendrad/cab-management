package com.interstatecab

import akka.http.scaladsl.model.{ContentTypes, StatusCodes}
import akka.http.scaladsl.server.Directives.handleExceptions
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{Matchers, WordSpec}
import com.interstatecab

class CabManagementRoutesTest extends WordSpec with Matchers with ScalatestRouteTest {

  val route = Route.seal(handleExceptions(CabServiceExceptionHandler()) {
    val service = new CabManagementService
    new CabManagementRoute(service).route
  })

  "Cab Management service" should {
    "on board a city" in {
      val path = s"/cab-management/v1/city"
      val requestBody = """{"cities":[{"id":"PUNE","name":"Pune"},{"id":"BANGALORE","name":"Bangalore"}]}"""
      val request = Post(path).withEntity(ContentTypes.`application/json`, requestBody)

      request ~> route ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[String] shouldEqual """{"cities":[{"id":"PUNE","name":"Pune"},{"id":"BANGALORE","name":"Bangalore"}]}"""
      }
    }

    "throw error if cab is being registered in unsupported city" in {
      val path = s"/cab-management/v1/cab"
      val requestBody = """{"cab_id": "MH12OP3245", "location_city_id": "DELHI", "state":"IDLE"}"""
      val request = Post(path).withEntity(ContentTypes.`application/json`, requestBody)

      request ~> route ~> check {
        status shouldEqual StatusCodes.BadRequest
        responseAs[String] shouldEqual """{"message":"Invalid city id DELHI"}"""
      }
    }

  }
}
