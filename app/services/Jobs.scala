package services

import akka.actor.{Actor, ActorLogging, Props, ActorSystem}
import play.api.Logger
import susy.susy

object ScheduledJobs {
  def props(susy: susy)(implicit system: ActorSystem): Props =
    Props.create(classOf[Jobs], susy)
  case object handleActions
//  val handleActions = "handle actions"
}

class Jobs(susy: susy) extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)


  override def receive: Receive = {
    case ScheduledJobs.handleActions =>
      logger.info("Handling actions proposals...")
      susy.attachData()
  }
}
