package network

import org.ergoplatform.appkit.{Address, BlockchainContext, ErgoClient, InputBox}

import javax.inject.{Inject, Singleton}
import ibportGateway.IBPortContracts
import luportGateway.LUPortContracts
import gateway.GatewayContracts


import scala.collection.JavaConverters._

@Singleton
class NetworkIObject @Inject()() {
  var client: ErgoClient = _
  var ibportContractsInterface: Option[IBPortContracts] = None
  var luportContractsInterface: Option[LUPortContracts] = None
  var gatewayContractsInterface: Option[GatewayContracts] = None



  def getCtxClient[T](f: BlockchainContext => T): T = {
    client.execute { ctx =>
      f(ctx)
    }
  }

  /**
   * @return current height of the blockchain
   */
  def getHeight: Long = {
    getCtxClient(ctx => ctx.getHeight)
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes
   */
  def getUnspentBox(address: Address): List[InputBox] = {
    getCtxClient(ctx =>
      ctx.getUnspentBoxesFor(address).asScala.toList
    )
  }

}
