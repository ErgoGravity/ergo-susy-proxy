package network

import gateway.GatewayContracts
import helpers.Configs
import ibportGateway.IBPortContracts
import javax.inject.Inject
import luportGateway.LUPortContracts
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
      networkIObject.client = RestApiErgoClient.create(Configs.nodeUrl, Configs.networkType, Configs.nodeApiKey, Configs.explorerUrl)
      networkIObject.getCtxClient(implicit ctx => {
        networkIObject.gatewayContractsInterface = Some(new GatewayContracts())
        networkIObject.luportContractsInterface = Some(new LUPortContracts(ctx))
        networkIObject.ibportContractsInterface = Some(new IBPortContracts(ctx))
        ctx.getHeight
      })

    } catch {
      case e: Throwable =>
        logger.error(s"Could not set client! ${e.getMessage}.")
        0L
    }
  }
}
