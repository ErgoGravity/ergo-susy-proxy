package susy.ibport

import javax.inject.Inject
import play.api.Logger

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.security.SecureRandom
import helpers.{Configs, Utils}
import network.{Explorer, NetworkIObject}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, InputBox, JavaHelpers, OutBox}
import play.api.libs.json.{JsValue, Json}
import special.collection.Coll

import java.io.PrintWriter
import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class IBPort @Inject()(utils: Utils, networkIObject: NetworkIObject, explorer: Explorer) {

  private val logger: Logger = Logger(this.getClass)

  private def selectRandomBox(seq: Seq[InputBox]): Option[InputBox] = {
    val random = new SecureRandom()
    new scala.util.Random(random).shuffle(seq).headOption
  }

  private def getSpecBox(typeBox: String, random: Boolean = false): InputBox = {
    val gatewayAddresses: IBPortContracts = networkIObject.ibportContractsInterface.get
    val boxData = typeBox match {
      case "linkList" =>
        ("linkList", gatewayAddresses.linkListAddress, Configs.ibportLinklistTokenId)
      case "maintainer" =>
        ("maintainer", gatewayAddresses.maintainerAddress, Configs.ibportMaintainerTokenId)
      case "linkListElement" =>
        ("linkListElement", gatewayAddresses.linkListElementAddress, Configs.ibportLinklistRepoTokenId)
      case "tokenRepo" =>
        ("tokenRepo", Configs.tokenRepoAddress, Configs.tokenRepoTokenId)
      case "proxy" =>
        ("proxy", Configs.proxyAddress.getErgoAddress.toString, "")
    }

    val boxes = networkIObject.getUnspentBox(Address.create(boxData._2))
    val box = if (boxData._1.equals("proxy"))
      boxes.filter(box => box.getTokens.size() == 0 && box.getValue > Configs.defaultTxFee * 4)
    else
      boxes.filter(box => box.getTokens.size() > 0 &&
        box.getTokens.get(0).getId.toString.equals(boxData._3) &&
        (if (boxData._1.equals("tokenRepo")) box.getTokens.get(0).getValue >= 2 else true))

    if (random) selectRandomBox(box).orNull else box.headOption.orNull
  }

  // TODO: approve function must be implemented

  def mint(signalBox: InputBox): Unit = {
    val maintainerBox = getSpecBox("maintainer")
    println(maintainerBox.getTokens)
    val tokenRepoBox = getSpecBox("tokenRepo", random = true)
    println(tokenRepoBox.getTokens)
    val proxyBox = getSpecBox("proxy", random = true)
    println(proxyBox.getTokens)

    val data = signalBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
    var amount = ByteBuffer.wrap(data.slice(33, 65).toArray).order(ByteOrder.BIG_ENDIAN).getLong()
    def createMaintainerBox(lastRepoBox: InputBox): OutBox = {
      networkIObject.getCtxClient(implicit ctx => {

        val txB = ctx.newTxBuilder()
        var tokenAmount = 0L
        var boxValue = 0L
        var newTxB = txB.outBoxBuilder()

        if (lastRepoBox.getTokens.size() > 1) {
          tokenAmount = lastRepoBox.getTokens.get(1).getValue - amount
          boxValue = lastRepoBox.getValue
          val newMaintainerRepoBox = newTxB.value(boxValue)
            .tokens(lastRepoBox.getTokens.get(0),
              new ErgoToken(lastRepoBox.getTokens.get(1).getId, tokenAmount))
            .registers(lastRepoBox.getRegisters.get(0))
            .contract(new ErgoTreeContract(Address.create(networkIObject.ibportContractsInterface.get.maintainerAddress).getErgoAddress.script))
            .build()
          newMaintainerRepoBox
        }
        else {
          boxValue = lastRepoBox.getValue - amount
          val newMaintainerRepoBox = newTxB.value(boxValue)
            .tokens(lastRepoBox.getTokens.get(0))
            .registers(lastRepoBox.getRegisters.get(0))
            .contract(new ErgoTreeContract(Address.create(networkIObject.ibportContractsInterface.get.maintainerAddress).getErgoAddress.script))
            .build()
          newMaintainerRepoBox
        }
      })
    }

    def createReceiverBox(maintainerBox: InputBox): OutBox = {
      println(s"amount: ${amount}")
      val receiver = (data.slice(65, data.size).toArray.map(_.toChar)).mkString
      println(s"receiver: ${receiver}")
      networkIObject.getCtxClient(implicit ctx => {
        val txB = ctx.newTxBuilder()
        var tokenAmount = 0L
        var boxValue = 0L
        val newTxB = txB.outBoxBuilder()
        if (maintainerBox.getTokens.size() > 1) {
          tokenAmount = amount
          boxValue = Configs.defaultTxFee
          val newTokenRepoBox = newTxB.value(boxValue)
            .tokens(new ErgoToken(maintainerBox.getTokens.get(1).getId, tokenAmount))
            .contract(new ErgoTreeContract(Address.create(receiver).getErgoAddress.script))
            .build()
          newTokenRepoBox
        }
        else {
          boxValue = Configs.defaultTxFee + amount
          val newTokenRepoBox = newTxB.value(boxValue)
            .contract(new ErgoTreeContract(Address.create(receiver).getErgoAddress.script))
            .build()
          newTokenRepoBox
        }
      })
    }

    def createTokenRepoBox(lastRepoBox: InputBox): OutBox = {
      networkIObject.getCtxClient(implicit ctx => {
        val txB = ctx.newTxBuilder()
        val newTokenRepoBox = txB.outBoxBuilder()
          .value(lastRepoBox.getValue + Configs.signalBoxValue)
          .tokens(new ErgoToken(lastRepoBox.getTokens.get(0).getId, lastRepoBox.getTokens.get(0).getValue + 1))
          .contract(new ErgoTreeContract(Address.create(Configs.tokenRepoAddress).getErgoAddress.script))
          .build()
        newTokenRepoBox
      })
    }

    networkIObject.getCtxClient(implicit ctx => {
      try {
        val prover = ctx.newProverBuilder()
          .withDLogSecret(Configs.proxySecret)
          .build()
        val outputs: Seq[OutBox] = Seq(createTokenRepoBox(tokenRepoBox),
          createMaintainerBox(maintainerBox), createReceiverBox(maintainerBox))
        val txB = ctx.newTxBuilder()
        val tx = txB.boxesToSpend(Seq(signalBox, tokenRepoBox, maintainerBox, proxyBox).asJava)
          .fee(2 * Configs.defaultTxFee)
          .outputs(outputs: _*)
          .sendChangeTo(Configs.proxyAddress.getErgoAddress)
          .build()
        val signed = prover.sign(tx)
        logger.debug(s"mint signed data ${signed.toJson(false)}")
        //        println(signed.toJson(false))
        new PrintWriter(s"logs/mint.txt") {
          write(signed.toJson(false))
          close()
        }
        val txId = ctx.sendTransaction(signed)
        logger.info(s"sending mint tx $txId")
        println(txId)
      }
      catch {
        case e: Exception => {
          logger.error(s"Failed to mint token: ${e}")
        }
      }
    })
  }

  def getLinkListElements: ListBuffer[Map[String, String]] = {
    try {
      val boxData =
        ("linkListElement", networkIObject.ibportContractsInterface.get.linkListElementAddress, Configs.ibportLinklistRepoTokenId)
      val boxes = networkIObject.getUnspentBox(Address.create(boxData._2))
        .filter(box => box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(boxData._3))
      val data = ListBuffer[Map[String, String]]()
      for (box <- boxes) {
        val boxReceiver = box.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toArray
        val receiver = (boxReceiver.map(_.toChar)).mkString
        val amount = box.getRegisters.get(1).getValue.asInstanceOf[Long].toString
        val reqId = box.getRegisters.get(2).getValue.asInstanceOf[special.sigma.BigInt]
        val requestId = JavaHelpers.SigmaDsl.toBigInteger(reqId).toString

        val value = Map("requestId" -> requestId, "amount" -> amount, "receiver" -> receiver)
        data += value
      }
      data
    }
    catch {
      case e: Exception => throw e
    }
  }

  def getAllRequestIds: ListBuffer[String] = {
    try {
      networkIObject.getCtxClient(implicit ctx => {
        val signalBoxes = Json.parse(explorer.getBoxes(Configs.signalAddress).toString())
        val signalBoxList = (signalBoxes \ "items").as[List[JsValue]]
        var requestIds = new ListBuffer[String]()
        signalBoxList.foreach(txJson => {
          val registers = (txJson \ "additionalRegisters").as[JsValue]
          val data = ((registers \ "R5").as[JsValue] \ "renderedValue").as[String]
          val dataArray = utils.toByteArray(data)
          val action = dataArray.slice(0, 1).map(_.toChar).mkString
          if (action == "m") {
            val reqId = dataArray.slice(1, 33)
            val requestId = BigInt(reqId).bigInteger.toString
            requestIds += requestId
          }
        })
        requestIds
      })
    } catch {
      case e: Exception => {
        println(e)
        logger.error(s"Failed to getAllRequests: ${e}")
        throw e
      }
    }
  }

}
