package network

import helpers.Configs
import javax.inject.Inject
import org.ergoplatform.appkit.RestApiErgoClient

import play.api.Logger


class Client @Inject()(networkIObject: NetworkIObject) {
  private val logger: Logger = Logger(this.getClass)
  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Content-Type", "application/json"))

  /**
   * Sets client for the entire app when the app starts
   *
   * @return current height of blockchain
   */
  def setClient(): Long = {
    try {
      networkIObject.client = RestApiErgoClient.create(Configs.nodeUrl, Configs.networkType, Configs.nodeApiKey)
      networkIObject.getCtxClient(implicit ctx => {
        ctx.getHeight
      })

    } catch {
      case e: Throwable =>
        logger.error(s"Could not set client! ${e.getMessage}.")
        0L
    }
  }
}