package susy.luport

import helpers.{Configs, Utils}
import network.{Explorer, NetworkIObject}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, InputBox, JavaHelpers, OutBox}
import play.api.Logger
import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._
import special.collection.Coll

import java.security.SecureRandom
import javax.inject.Inject
import scala.util.control.Breaks.breakable

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
      boxes.filter(box => box.getValue > Configs.defaultTxFee * 2)
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
      var amount = lastRepoBox.getRegisters.get(5).getValue.asInstanceOf[Long]
      amount = amount + fee * amount / 10000
      networkIObject.getCtxClient(implicit ctx => {
        val txB = ctx.newTxBuilder()
        var newTokenRepoBox = txB.outBoxBuilder()
        if (lastRepoBox.getTokens.size() > 1) {
          newTokenRepoBox = newTokenRepoBox.tokens(new ErgoToken(lastRepoBox.getTokens.get(0).getId, 1),
            new ErgoToken(lastRepoBox.getTokens.get(1).getId, lastRepoBox.getTokens.get(1).getValue - amount))
          newTokenRepoBox = newTokenRepoBox.value(lastRepoBox.getValue)
        }
        else {
          newTokenRepoBox = newTokenRepoBox.tokens(new ErgoToken(lastRepoBox.getTokens.get(0).getId, 1))
          newTokenRepoBox = newTokenRepoBox.value(lastRepoBox.getValue - amount)
        }

        newTokenRepoBox.contract(new ErgoTreeContract(Address.create(networkIObject.luportContractsInterface.get.maintainerAddress).getErgoAddress.script))
        newTokenRepoBox.build()
      })
    }

    def createReceiverBox(signalBox: InputBox, maintainerBox: InputBox): OutBox = {
      val data = signalBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
      val fee = maintainerBox.getRegisters.get(0).getValue.asInstanceOf[Int]
      var amount = data.slice(33, 65).toString().toLong
      amount = amount + fee * amount / 10000
      val receiver = data.slice(66, data.size).toString
      networkIObject.getCtxClient(implicit ctx => {
        val txB = ctx.newTxBuilder()
        var newTokenRepoBox = txB.outBoxBuilder()
        if (maintainerBox.getTokens.size() > 1) {
          newTokenRepoBox = newTokenRepoBox.tokens(new ErgoToken(maintainerBox.getTokens.get(0).getId, 1),
            new ErgoToken(maintainerBox.getTokens.get(1).getId, maintainerBox.getTokens.get(1).getValue - amount))
          newTokenRepoBox = newTokenRepoBox.value(maintainerBox.getValue)
        }
        else {
          newTokenRepoBox = newTokenRepoBox.tokens(new ErgoToken(maintainerBox.getTokens.get(0).getId, 1))
          newTokenRepoBox = newTokenRepoBox.value(maintainerBox.getValue - amount)
        }
        newTokenRepoBox.contract(new ErgoTreeContract(Address.create(receiver).getErgoAddress.script))
        newTokenRepoBox.build()
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
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.proxySecret)
        .build()
      val outputs: Seq[OutBox] = Seq(createTokenRepoBox(tokenRepoBox),
        createMaintainerBox(maintainerBox), createReceiverBox(signalBox, maintainerBox), createProxyBox(proxyBox))
      val txB = ctx.newTxBuilder()
      val tx = txB.boxesToSpend(Seq(signalBox, tokenRepoBox, maintainerBox, proxyBox).asJava)
        .fee(Configs.defaultTxFee)
        .outputs(outputs: _*)
        .sendChangeTo(Configs.proxyAddress.getErgoAddress)
        .withDataInputs(Seq(lastOracleBox).toList.asJava)
        .build()
      val signed = prover.sign(tx)
      logger.debug(s"pulseTx data ${signed.toJson(false)}")
      val pulseTxId = ctx.sendTransaction(signed)
      logger.info(s"sending pulse tx $pulseTxId")
      pulseTxId
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

  def getRequest(requestId: String): Map[String, String] = {
    println(requestId)
    try {
      val boxes = networkIObject.getUnspentBox(Address.create(networkIObject.luportContractsInterface.get.linkListElementAddress))
      val box = boxes.filter(box => box.getRegisters.get(2).getValue.asInstanceOf[special.sigma.BigInt] == JavaHelpers.SigmaDsl.BigInt(BigInt(requestId).bigInteger)).head
      val receiver = box.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toArray
      val boxReceiver = (receiver.map(_.toChar)).mkString
      val boxAmount = box.getRegisters.get(1).getValue.asInstanceOf[Long].toString
      val boxRequestId = requestId
      Map("requestId" -> boxRequestId, "amount" -> boxAmount, "receiver" -> boxReceiver)
    } catch {
      case e: Exception => {
        println(e)
        Map("requestId" -> "", "amount" -> "", "receiver" -> "")
      }
    }
  }
}
