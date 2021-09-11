package controllers

import javax.inject._
import network.NetworkIObject
import play.api.Logger
import play.api.mvc._
import io.circe.syntax._
import helpers.{Configs, Utils}
import play.api.libs.circe.Circe
import susy.ibport.IBPort
import susy.luport.LUPort

import scala.concurrent.ExecutionContext

/**
 * Controller of Ergo-Susy-Proxy
 */
class ApiController @Inject()(controllerComponents: ControllerComponents,
                              utils: Utils, networkIObject: NetworkIObject, ibport: IBPort, luport: LUPort)
                             (implicit ec: ExecutionContext) extends AbstractController(controllerComponents) with Circe {

  private val logger: Logger = Logger(this.getClass)

  def exception(e: Throwable): Result = {
    logger.error(s"error in controller ${utils.getStackTraceStr(e)}")
    BadRequest(s"""{"success": false, "message": "${e.getMessage}"}""").as("application/json")
  }

  /**
   * @return current a text for root API
   */
  def index: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "message": "Ergo Susy Proxy"
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
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

  /**
   * @return current list of link list element of on ibport
   */
  def getibportState: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "state": ${ibport.getLinkListElements.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return current list of link list element of on luport
   */
  def getluportState: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "state": ${luport.getLinkListElements.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  def getIBPortDetails: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      networkIObject.getCtxClient(implicit ctx => {
        val ibPortObject = networkIObject.ibportContractsInterface.get
        val contractAddreses: Map[String, String] = Map(
          "maintainerAddress" -> ibPortObject.maintainerAddress,
          "linkListAddress" -> ibPortObject.linkListAddress,
          "linkListElementAddress" -> ibPortObject.linkListElementAddress,
          "signalAddress" -> Configs.signalAddress,
          "tokenRepoAddress" -> Configs.tokenRepoAddress,
        )

        val tokenIds: Map[String, String] = Map(
          "linklistRepoTokenId" -> Configs.ibportLinklistRepoTokenId,
          "linklistTokenId" -> Configs.ibportLinklistTokenId,
          "maintainerTokenId" -> Configs.ibportMaintainerTokenId,
          "gwTokenId" -> Configs.ibportGWTokenId,
          "tokenRepoTokenId" -> Configs.tokenRepoTokenId)

        Ok(
          s"""{
             |  "success": true,
             |  "contractAddreses": ${contractAddreses.asJson},
             |  "tokenIds": ${tokenIds.asJson}
             |}""".stripMargin
        ).as("application/json")
      })
    } catch {
      case e: Throwable => exception(e)
    }
  }

  def getLUPortDetails: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      networkIObject.getCtxClient(implicit ctx => {
        val luPortObject = networkIObject.luportContractsInterface.get
        val contractAddreses: Map[String, String] = Map(
          "maintainerAddress" -> luPortObject.maintainerAddress,
          "linkListAddress" -> luPortObject.linkListAddress,
          "linkListElementAddress" -> luPortObject.linkListElementAddress,
          "signalAddress" -> Configs.signalAddress,
          "tokenRepoAddress" -> Configs.tokenRepoAddress,
        )

        val tokenIds: Map[String, String] = Map(
          "linklistRepoTokenId" -> Configs.luportLinklistRepoTokenId,
          "linklistTokenId" -> Configs.luportLinklistTokenId,
          "maintainerTokenId" -> Configs.luportMaintainerTokenId,
          "tokenId" -> Configs.luportTokenId,
          "tokenRepoTokenId" -> Configs.tokenRepoTokenId)
        Ok(
          s"""{
             |  "success": true,
             |  "contractAddreses": ${contractAddreses.asJson},
             |  "tokenIds": ${tokenIds.asJson}
             |}""".stripMargin
        ).as("application/json")
      })
    } catch {
      case e: Throwable => exception(e)
    }
  }
}

