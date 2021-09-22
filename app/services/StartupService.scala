package services

import javax.inject._
import play.api.Logger
import play.api.inject.ApplicationLifecycle

import akka.actor.{ActorRef, ActorSystem, Props}
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import network.Client
import susy.susy

@Singleton
class StartupService @Inject()(appLifecycle: ApplicationLifecycle, implicit val system: ActorSystem,
                               client: Client, susy: susy)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  client.setClient()

  lazy val jobsActor: ActorRef = system.actorOf(Props(new Jobs(susy)), "scheduling-jobs-actor")
  def onStart(): Unit = {
    logger.info("App started!")
    system.scheduler.scheduleAtFixedRate(
      initialDelay = 10.seconds,
      interval = 300.seconds,
      receiver = jobsActor,
      message = ScheduledJobs.handleActions
    )
  }

  def onShutdown(): Unit = {
    system.stop(jobsActor)
    logger.info("Shutting down")
  }

  appLifecycle.addStopHook { () =>
    onShutdown()
    logger.info("App stopped!")
    Future.successful(())
  }
  onStart()
}
