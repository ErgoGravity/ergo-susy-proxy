package services

import akka.actor.ActorSystem
import javax.inject._
import play.api.Logger
import play.api.inject.ApplicationLifecycle

import akka.actor.{ActorRef, ActorSystem, Props}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import network.Client
import gateway.Gateway

@Singleton
class StartupService @Inject()(appLifecycle: ApplicationLifecycle, system: ActorSystem,
                               node: Client, gateway: Gateway)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()

  val jobsActor: ActorRef = system.actorOf(Props(new Jobs(gateway)), "scheduling-jobs-actor")
  system.scheduler.scheduleAtFixedRate(
    initialDelay = 5.seconds,
    interval = 300.seconds,
    receiver = jobsActor,
    message = Jobs.handleActions
  )

  appLifecycle.addStopHook { () =>
    logger.info("App stopped!")
    Future.successful(())
  }
}
