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
import helpers.Utils
import io.circe.Json

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
  def getIBPortRequestsList: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "requests": ${ibport.getLinkListElements.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  /**
   * @return current list of link list element of on luport
   */
  def getLUPortRequestsList: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "requests": ${luport.getLinkListElements.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  def getIBPortRequestIds: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "requests": ${ibport.getAllRequestIds.asJson}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }

  def getLUPortRequestIds: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "success": true,
           |  "requests": ${luport.getAllRequestIds.asJson}
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
          "tokenRepoAddress" -> Configs.tokenRepoAddress
        )

        val tokenIds: Map[String, String] = Map(
          "linkListRepoTokenId" -> Configs.ibportLinklistRepoTokenId,
          "linkListTokenId" -> Configs.ibportLinklistTokenId,
          "maintainerTokenId" -> Configs.ibportMaintainerTokenId,
          "gwTokenId" -> Configs.ibportGWTokenId,
          "tokenRepoTokenId" -> Configs.tokenRepoTokenId
        )

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
          "tokenRepoAddress" -> Configs.tokenRepoAddress
        )

        val tokenIds: Map[String, String] = Map(
          "linkListRepoTokenId" -> Configs.luportLinklistRepoTokenId,
          "linkListTokenId" -> Configs.luportLinklistTokenId,
          "maintainerTokenId" -> Configs.luportMaintainerTokenId,
          "tokenId" -> Configs.luportTokenId,
          "tokenRepoTokenId" -> Configs.tokenRepoTokenId
        )
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



  def validateAddress(): Action[Json] = Action(circe.json) { implicit request =>
    try {
      val address = request.body.hcursor.downField("address").as[String].getOrElse(throw new Throwable("address field must exist"))
      Ok(
        s"""{
           |  "success": true,
           |  "isValid": ${utils.getAddress(address)}
           |}""".stripMargin
      ).as("application/json")

    } catch {
      case e: Throwable => exception(e)
    }
  }
}

