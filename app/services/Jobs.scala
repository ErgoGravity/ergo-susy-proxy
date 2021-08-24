package services

import akka.actor.{Actor, ActorLogging, Props, ActorSystem}
import play.api.Logger
import susy.susy

object Jobs {
  def props(susy: susy)(implicit system: ActorSystem): Props =
    Props.create(classOf[Jobs], susy)

  val handleActions = "handle actions"
}

class Jobs(susy: susy) extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)


  def receive: Receive = {
    case Jobs.handleActions =>
      logger.info("Handling actions proposals...")
      susy.attachData()
  }
}
