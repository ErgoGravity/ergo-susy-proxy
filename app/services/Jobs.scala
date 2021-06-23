package services

import akka.actor.{Actor, ActorLogging, Props, ActorSystem}
import play.api.Logger
import gateway.Gateway
import play.api.libs.json.{JsValue, Json}

import scala.util.Try

object Jobs {
  def props(gateway: Gateway)(implicit system: ActorSystem): Props =
    Props.create(classOf[Jobs], gateway)

  val handleActions = "handle actions"
}

class Jobs(gateway: Gateway) extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)


  def receive = {
    case Jobs.handleActions =>
      logger.info("Handling actions proposals...")
      gateway.attachData()
  }
}
