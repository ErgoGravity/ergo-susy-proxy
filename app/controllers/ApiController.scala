package controllers

import javax.inject._
import network.NetworkIObject
import play.api.Logger
import play.api.mvc._
import helpers.Utils
import play.api.libs.circe.Circe

import scala.concurrent.ExecutionContext

/**
 * Controller of Ergo-Susy-Proxy
 */
class ApiController @Inject()(controllerComponents: ControllerComponents,
                              utils: Utils, networkIObject: NetworkIObject)
                             (implicit ec: ExecutionContext) extends AbstractController(controllerComponents) with Circe {

  private val logger: Logger = Logger(this.getClass)

  def exception(e: Throwable): Result = {
    logger.error(s"error in controller ${utils.getStackTraceStr(e)}")
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }

  /**
   * @return current height of the blockchain
   */
  def height: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "height": ${networkIObject.getHeight}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

}

