package gateway

import helpers.{Configs, Utils}
import luportGateway.LUPortContracts
import network.NetworkIObject
import org.ergoplatform.appkit.{Address, InputBox}
import play.api.Logger
import special.collection.Coll
import ibportGateway.IBPort

import luportGateway.LUPort

import java.security.SecureRandom
import javax.inject.Inject


class  Gateway @Inject()(utils: Utils, networkIObject: NetworkIObject, luPort: LUPort, ibPort: IBPort){

  private val logger: Logger = Logger(this.getClass)
  var gatewayAddresses: GatewayContracts = _

  def this(utils: Utils, networkIObject: NetworkIObject,
           gatewayAddresses: GatewayContracts, luPort: LUPort, ibPort: IBPort ) = {
    this(utils, networkIObject, luPort, ibPort)
    this.gatewayAddresses = networkIObject.gatewayContractsInterface.get
  }

  def getSignalList(): List[InputBox] = {
    val boxData = ("signal", gatewayAddresses.signalAddress, LUPortContracts.tokenRepoTokenId)

    networkIObject.getUnspentBox(Address.create(boxData._2))
      .filter(box => box.getTokens.size() > 0 &&
        box.getTokens.get(0).getId.toString.equals(boxData._3))
  }

  def attachData(): Unit = {
    val signalList = getSignalList()
    for (signal <- signalList) {
      val data = signal.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
      val action = data.slice(0, 1).toString()
      if (action == "u") luPort.unlock(signal)
      if (action == "m") ibPort.mint(signal)
      // TODO: approve and changeState must be added
    }
  }

}
