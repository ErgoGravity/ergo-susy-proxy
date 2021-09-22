package susy

import helpers.Configs
import network.NetworkIObject
import org.ergoplatform.appkit.{Address, InputBox}
import play.api.Logger
import special.collection.Coll
import susy.ibport.IBPort
import susy.luport.LUPort

import javax.inject.Inject


class susy @Inject()(networkIObject: NetworkIObject, luPort: LUPort, ibPort: IBPort) {

  private val logger: Logger = Logger(this.getClass)

  // TODO: change the way of getting signal list
  def getSignalList: List[InputBox] = {
    val boxData = ("signal", Configs.signalAddress, Configs.tokenRepoTokenId)
    println("getSignalList")
    try {
      networkIObject.getUnspentBox(Address.create(boxData._2))
        .filter(box => box.getTokens.size() > 0 &&
          box.getTokens.get(0).getId.toString.equals(boxData._3))
    }
    catch {
      case e: Exception => {
        println(e)
        List()
      }
    }
  }

  def attachData(): Unit = {
    println("Attaching data")
    val signalList = getSignalList
    for (signal <- signalList) {
      val data = signal.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
      println(data.size)
      println(s"data ${data}")
      val action = (data.slice(0, 1).toArray.map(_.toChar)).mkString
      println(action)
      if (action == "u") luPort.unlock(signal)
      if (action == "m") ibPort.mint(signal)
      // TODO: approve and changeState must be added
    }
  }

}
