package susy.luport

import helpers.{Configs, Utils}
import network.{Explorer, NetworkIObject}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, InputBox, JavaHelpers, OutBox}
import play.api.Logger
import play.api.libs.json.{JsValue, Json}
import java.nio.ByteBuffer
import java.nio.ByteOrder
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import special.collection.Coll

import java.security.SecureRandom
import javax.inject.Inject

class LUPort @Inject()(utils: Utils, networkIObject: NetworkIObject, explorer: Explorer) {
  private val logger: Logger = Logger(this.getClass)

  private def selectRandomBox(seq: Seq[InputBox]): Option[InputBox] = {
    val random = new SecureRandom()
    new scala.util.Random(random).shuffle(seq).headOption
  }

  private def getSpecBox(typeBox: String, random: Boolean = false): InputBox = {
    val gatewayAddresses: LUPortContracts = networkIObject.luportContractsInterface.get
    val boxData = typeBox match {
      case "linkList" =>
        ("linkList", gatewayAddresses.linkListAddress, Configs.luportLinklistTokenId)
      case "maintainer" =>
        ("maintainer", gatewayAddresses.maintainerAddress, Configs.luportMaintainerTokenId)
      case "linkListElement" =>
        ("linkListElement", gatewayAddresses.linkListElementAddress, Configs.luportLinklistRepoTokenId)
      case "proxy" =>
        ("proxy", Configs.proxyAddress.getErgoAddress.toString, "")
      case "tokenRepo" =>
        ("tokenRepo", Configs.tokenRepoAddress, Configs.tokenRepoTokenId)
    }

    val boxes = networkIObject.getUnspentBox(Address.create(boxData._2))
    val box = if (boxData._1.equals("proxy"))
      boxes.filter(box => box.getValue > Configs.defaultTxFee * 4)
    else
      boxes.filter(box => box.getTokens.size() > 0 &&
        box.getTokens.get(0).getId.toString.equals(boxData._3) &&
        (if (boxData._1.equals("tokenRepo")) box.getTokens.get(0).getValue >= 2 else true))

    if (random) selectRandomBox(box).orNull else box.headOption.orNull
  }

  // TODO: approve function must be implemented

  def unlock(signalBox: InputBox): Unit = {
    val maintainerBox = getSpecBox("maintainer")
    val lastOracleBox = getSpecBox("oracle")
    val tokenRepoBox = getSpecBox("tokenRepo", random = true)
    val proxyBox = getSpecBox("proxy", random = true)

    def createMaintainerBox(lastRepoBox: InputBox): OutBox = {
      val fee = lastRepoBox.getRegisters.get(0).getValue.asInstanceOf[Int]
      val data = signalBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
      var amount = BigInt(data.slice(33, 65).toArray).toLong
      amount = amount + fee * amount / 10000
      networkIObject.getCtxClient(implicit ctx => {
        val txB = ctx.newTxBuilder()
        var tokenAmount = 0L
        var boxValue = 0L
        var newTxB = txB.outBoxBuilder()
        if (lastRepoBox.getTokens.size() > 1) {
          tokenAmount = lastRepoBox.getTokens.get(1).getValue - amount
          boxValue = lastRepoBox.getValue
          val newTokenRepoBox = newTxB.value(boxValue)
            .tokens(lastRepoBox.getTokens.get(0),
              new ErgoToken(lastRepoBox.getTokens.get(1).getId, tokenAmount))
            .registers(lastRepoBox.getRegisters.get(0))
            .contract(new ErgoTreeContract(Address.create(networkIObject.ibportContractsInterface.get.maintainerAddress).getErgoAddress.script))
            .build()
          newTokenRepoBox
        }
        else {
          boxValue = lastRepoBox.getValue - amount
          val newTokenRepoBox = newTxB.value(boxValue)
            .tokens(lastRepoBox.getTokens.get(0))
            .registers(lastRepoBox.getRegisters.get(0))
            .contract(new ErgoTreeContract(Address.create(networkIObject.ibportContractsInterface.get.maintainerAddress).getErgoAddress.script))
            .build()
          newTokenRepoBox
        }
      })
    }

    def createReceiverBox(signalBox: InputBox, maintainerBox: InputBox): OutBox = {
      val data = signalBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
      val fee = maintainerBox.getRegisters.get(0).getValue.asInstanceOf[Int]
      var amount = BigInt(data.slice(33, 65).toArray).toLong
      println(amount)
      amount = amount - fee * amount / 10000
      val receiver = (data.slice(65, data.size).toArray.map(_.toChar)).mkString
      println(receiver)
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
        var newTokenRepoBox = txB.outBoxBuilder()
        newTokenRepoBox = newTokenRepoBox.value(lastRepoBox.getValue + Configs.signalBoxValue)
        newTokenRepoBox = newTokenRepoBox.tokens(new ErgoToken(lastRepoBox.getTokens.get(0).getId, lastRepoBox.getTokens.get(0).getValue + 1))
        newTokenRepoBox.contract(new ErgoTreeContract(Address.create(Configs.tokenRepoAddress).getErgoAddress.script))
        newTokenRepoBox.build()
      })
    }

    def createProxyBox(proxyBox: InputBox): OutBox = {
      networkIObject.getCtxClient(implicit ctx => {
        val txB = ctx.newTxBuilder()
        var newProxyBox = txB.outBoxBuilder()
        newProxyBox = newProxyBox.value(proxyBox.getValue - Configs.defaultTxFee)
        newProxyBox = newProxyBox.tokens(proxyBox.getTokens.asScala.toList: _*)
        newProxyBox.contract(new ErgoTreeContract(Configs.proxyAddress.getErgoAddress.script))
        newProxyBox.build()
      })
    }

    networkIObject.getCtxClient(implicit ctx => {
      try {
        val prover = ctx.newProverBuilder()
          .withDLogSecret(Configs.proxySecret)
          .build()
        val outputs: Seq[OutBox] = Seq(createTokenRepoBox(tokenRepoBox),
          createMaintainerBox(maintainerBox), createReceiverBox(signalBox, maintainerBox))
        val txB = ctx.newTxBuilder()
        val tx = txB.boxesToSpend(Seq(signalBox, tokenRepoBox, maintainerBox, proxyBox).asJava)
          .fee(2 * Configs.defaultTxFee)
          .outputs(outputs: _*)
          .sendChangeTo(Configs.proxyAddress.getErgoAddress)
          .withDataInputs(Seq(lastOracleBox).toList.asJava)
          .build()
        val signed = prover.sign(tx)
        logger.debug(s"unlock signed data ${signed.toJson(false)}")
        val pulseTxId = ctx.sendTransaction(signed)
        logger.info(s"sending unlock tx $pulseTxId")
        pulseTxId
      }
      catch {
        case e: Exception => {
          logger.error(s"Failed to unlock token: ${e}")
        }
      }
    })
  }


  def getLinkListElements: ListBuffer[Map[String, String]] = {
    try {
      val boxData =
        ("linkListElement", networkIObject.luportContractsInterface.get.linkListElementAddress, Configs.luportLinklistRepoTokenId)
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
          if (action == "u") {
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
