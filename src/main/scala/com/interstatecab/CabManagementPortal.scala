package com.interstatecab

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives.{handleExceptions, handleRejections}
import akka.http.scaladsl.server.{RejectionHandler, RouteResult}
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory

/**
  *
  * Problem Definition​:
  * You have to build an inter city cab management portal to be used as an admin and booking tool
  * This portal should be able to :
  *1. Register cabs.
  *2. Onboard various cities where cab services are provided.
  *3. Change current city (location) of any cab.
  *4. Change state of any cab. For this you will have to define a state machine for the cab ex:
  * a cab must have at least these two basic states; IDLE and ON_TRIP
  *5. Book cabs based on their availability at a certain location. In case more than one cab are
  * available , use the following strategy;
  *a. Find out which cab has remained idle the most and assign it.
  *b. In case of clash above, randomly assign any cab
  * Assumption : a cab once assigned a trip cannot cancel/reject it
  **
  *Other Details​:
  *Input: a snapshot of all cabs with their metadata and location
  *a List of <Cab_Id, Cab_State, City_Id>
  *In case the Cab_State is ON_TRIP, the City_Id will be indeterminate
  */
object CabManagementPortal {

  val log = LoggerFactory.getLogger(this.getClass)
  val config = ConfigFactory.load()
  val apiHostName = "localhost"
  val apiPort = 8080

  log.info("Hello. This is the interstate cab Management portal....")

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem("app")
    implicit val materializer = ActorMaterializer()
    implicit val executionContext = system.dispatcher

    // Creating http service
    val service = new CabManagementService
    val routeObj = new CabManagementRoute(service)

    // Running akka http server
    val bindingFuture = {
      val handleErrors = handleRejections(RejectionHandler.default) & handleExceptions(CabServiceExceptionHandler.apply())
      val finalRoute = handleErrors { routeObj.route }
      Http().newServerAt(apiHostName, apiPort).bindFlow(RouteResult.routeToFlow(finalRoute))
    }
    log.info(s"Server online at http://$apiHostName:$apiPort")

    // register a shutdown hook
    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      log.info("Inside shutdown hook of job")
      bindingFuture.flatMap(_.unbind()) // trigger unbinding from the port
    }))
  }
}


