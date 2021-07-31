package ibportGateway

import javax.inject.Inject
import play.api.Logger

import java.security.SecureRandom
import helpers.{Configs, Utils}
import network.NetworkIObject
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox}
import special.collection.Coll

import scala.collection.JavaConverters._
import scala.collection.mutable.ListBuffer

class IBPort @Inject()(utils: Utils, networkIObject: NetworkIObject) {

  private val logger: Logger = Logger(this.getClass)

  private def selectRandomBox(seq: Seq[InputBox]): Option[InputBox] = {
    val random = new SecureRandom()
    new scala.util.Random(random).shuffle(seq).headOption
  }

  private def getSpecBox(typeBox: String, random: Boolean = false): InputBox = {
    val gatewayAddresses: IBPortContracts = networkIObject.ibportContractsInterface.get
    val boxData = typeBox match {
      case "linkList" =>
        ("linkList", gatewayAddresses.linkListAddress, IBPortContracts.linkListTokenId)
      case "maintainer" =>
        ("maintainer", gatewayAddresses.maintainerAddress, IBPortContracts.maintainerTokenId)
      case "linkListElement" =>
        ("linkListElement", gatewayAddresses.linkListElementAddress, IBPortContracts.linkListElementTokenId)
      case "proxy" =>
        ("proxy", Configs.proxyAddress.getErgoAddress.toString, "")
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

  def mint(signalBox: InputBox): Unit = {
    val maintainerBox = getSpecBox("maintainer")
    val linkListElementBox = getSpecBox("linkListElement")
    val lastOracleBox = getSpecBox("oracle")
    val tokenRepoBox = getSpecBox("tokenRepo", random = true)
    val proxyBox = getSpecBox("proxy", random = true)

    def createMaintainerBox(lastRepoBox: InputBox): OutBox = {
      val fee = lastRepoBox.getRegisters.get(0).getValue.asInstanceOf[Int]
      var amount = lastRepoBox.getRegisters.get(5).getValue.asInstanceOf[Int]
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

        newTokenRepoBox.contract(new ErgoTreeContract(Address.create(networkIObject.ibportContractsInterface.get.maintainerAddress).getErgoAddress.script))
        newTokenRepoBox.build()
      })
    }

    def createReceiverBox(signalBox: InputBox, maintainerBox: InputBox): OutBox = {
      val data = signalBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]]
      val fee = maintainerBox.getRegisters.get(0).getValue.asInstanceOf[Int]
      var amount = data.slice(32, 65).toString().toLong
      amount = amount + fee * amount / 10000
      val receiver = data.toArray.drop(65).toString
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
        newTokenRepoBox.contract(new ErgoTreeContract(Address.create(networkIObject.ibportContractsInterface.get.tokenRepoAddress).getErgoAddress.script))
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
      logger.info(s"sending pulse tx ${pulseTxId}")
      pulseTxId
    })
  }


  def getLinkListElements: ListBuffer[Map[String, String]] = {
    val boxData =
      ("linkListElement", networkIObject.ibportContractsInterface.get.linkListElementAddress, IBPortContracts.linkListElementTokenId)
    val boxes = networkIObject.getUnspentBox(Address.create(boxData._2))
      .filter(box => box.getTokens.size() > 0 && box.getTokens.get(0).getId.toString.equals(boxData._3))
    var data = ListBuffer[Map[String, String]]()
    for (box <- boxes) {
      val receiver = box.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toString()
      val amount = box.getRegisters.get(0).getValue.asInstanceOf[BigInt].toString()
      val requestId = box.getRegisters.get(0).getValue.asInstanceOf[BigInt].toString()

      val value = Map("requestId" -> requestId, "amount" -> amount, "receiver" -> receiver)
      data += value
    }
    data

  }
}
