package susy.luport

import helpers.Configs
import org.ergoplatform.appkit._
import scorex.crypto.hash.Digest32
import sigmastate.Values.ErgoTree


class LUPortContracts(ctx: BlockchainContext) {

  var linkListAddress: String = _
  var maintainerAddress: String = _
  var linkListElementAddress: String = _

  /**
   * inputs in CreateTransferWrapRq: 0-> linkListTokenRepo, 1-> maintainerRepo
   * outputs in CreateTransferWrapRq: 0-> linkListTokenRepo, 1-> linkListElementRepo , 2-> maintainerRepo
   *
   * inputs in Approve: 0-> Signal, 1-> Signal forced input, 2-> linkListTokenRepo, 3-> linkListElementRepo
   * outputs in Approve: 0-> Signal forced output (tokenRepo), 1-> linkListTokenRepo
   *
   * inputs in Unlock: 0-> Signal, 1-> Signal forced input, 2-> maintainerRepo
   * outputs in Unlock: 0-> Signal forced output (tokenRepo), 1-> maintainerRepo, 2-> receiver
   */
  lazy val linkListRepoScript: String =
    s"""{
       |  val check = {
       |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
       |      val linkListTokenOutput = OUTPUTS(0)
       |      val linkListElementOutput = OUTPUTS(1)
       |      allOf(Coll(
       |        INPUTS(1).tokens(0)._1 == maintainerNFTToken,
       |
       |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
       |        linkListTokenOutput.tokens(1)._2 == INPUTS(0).tokens(1)._2 - 1,
       |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
       |        linkListTokenOutput.R4[BigInt].isDefined, // last request Id
       |        linkListTokenOutput.R5[Int].isDefined, // nft count
       |        linkListTokenOutput.R6[Int].isDefined, // nft number
       |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
       |        linkListTokenOutput.value == INPUTS(0).value - minValue,//TODO : check minvalue
       |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
       |
       |        OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
       |      ))
       |    }
       |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // approve
       |      val linkListTokenOutput = OUTPUTS(1)
       |      allOf(Coll(
       |        linkListTokenOutput.tokens(1)._2 == INPUTS(2).tokens(1)._2 + 1,
       |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
       |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
       |        linkListTokenOutput.R4[BigInt].isDefined, // last request Id
       |        linkListTokenOutput.R5[Int].isDefined, // nft count
       |        linkListTokenOutput.R6[Int].isDefined, // nft number
       |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
       |        linkListTokenOutput.value == INPUTS(2).value + minValue,
       |
       |        INPUTS(2).propositionBytes == SELF.propositionBytes,
       |        INPUTS(2).id == SELF.id,
       |        blake2b256(INPUTS(3).propositionBytes) == linkListElementRepoContractHash
       |      ))
       |    }
       |    else false
       |  }
       |
       |  sigmaProp (check)
       |}""".stripMargin

  lazy val maintainerRepoScript: String =
    s"""{
       |val storeInMaintainer = {(v: ((Box, Box), (Int, Long) )) => {
       |    val amount = v._2._2 + v._2._1 * v._2._2 / 10000
       |    if (v._1._1.tokens.size > 1){
       |      allOf(Coll(
       |          v._1._2.value == v._1._1.value,
       |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
       |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + amount
       |      ))
       |    }
       |    else{
       |      allOf(Coll(
       |          v._1._2.value == v._1._1.value + amount
       |      ))
       |    }
       |  }}
       |
       |val unlock: Boolean = {(v: ((Box, Box), (Box, Long))) => {
       |  if (v._1._1.tokens.size > 1){
       |    allOf(Coll(
       |      v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
       |      v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2,
       |      v._1._2.value == v._1._1.value
       |      ))
       |    }
       |  else{
       |     allOf(Coll(
       |        v._1._2.value == v._1._1.value - v._2._2
       |     ))
       |    }
       |  }}
       |
       |val createTransferRequestScenario = {
       |  if(INPUTS(0).tokens(0)._1 == linkListNFTToken && INPUTS(0).R5[Int].isDefined){
       |    val linkListTokenRepo = OUTPUTS(0)
       |    val linkListElement = OUTPUTS(1)
       |    val maintainer = OUTPUTS(2)
       |    val fee = INPUTS(1).R4[Int].get
       |    val amount = OUTPUTS(1).R5[Long].get
       |
       |    allOf(Coll(
       |      INPUTS(0).tokens(0)._1 == linkListNFTToken,
       |      INPUTS(0).tokens(1)._1 == linkListTokenRepoId,
       |      INPUTS(1).propositionBytes == SELF.propositionBytes,
       |      INPUTS(1).id == SELF.id,
       |
       |      linkListTokenRepo.tokens(0)._1 == linkListNFTToken,
       |      blake2b256(linkListElement.propositionBytes) == linkListElementRepoContractHash,
       |
       |      maintainer.tokens(0)._1 == maintainerNFTToken,
       |      maintainer.tokens(1)._1 == maintainerRepoId,
       |      maintainer.propositionBytes == SELF.propositionBytes,
       |      maintainer.R4[Int].get == INPUTS(1).R4[Int].get,
       |      storeInMaintainer(( (INPUTS(1), OUTPUTS(2)), (fee, amount) ))
       |    ))}
       |   else false
       | }
       | val unlockScenario = {
       |  if(INPUTS(0).tokens(0)._1 == signalTokenNFT && INPUTS(0).R5[Coll[Byte]].isDefined){
       |    // OUTPUTS(0) -> tokenRepo
       |    val maintainer = OUTPUTS(1)
       |    // OUTPUTS(2) -> receiver
       |
       |    val data = INPUTS(0).R5[Coll[Byte]].get
       |    val amount = byteArrayToLong(data.slice(33, 65))
       |    val fee = INPUTS(2).R4[Int].get
       |    val newAmount = amount + fee * amount / 10000
       |
       |    allOf(Coll(
       |
       |      INPUTS(0).tokens(0)._1 == signalTokenNFT,
       |      INPUTS(2).propositionBytes == SELF.propositionBytes,
       |      INPUTS(2).id == SELF.id,
       |
       |      maintainer.tokens(1)._1 == maintainerRepoId,
       |      maintainer.tokens(0)._1 == maintainerNFTToken,
       |      maintainer.propositionBytes == SELF.propositionBytes,
       |      maintainer.R4[Int].get == INPUTS(2).R4[Int].get,
       |
       |      unlock(((INPUTS(2), maintainer), (OUTPUTS(2), newAmount)))
       |      //OUTPUTS(2).propositionBytes == receiver
       |    ))
       |   }
       |   else false
       | }
       |sigmaProp (createTransferRequestScenario || unlockScenario)
       |}""".stripMargin

  lazy val linkListElementScript: String =
    s"""{
       |  val check = {
       |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
       |      val linkListTokenOutput = OUTPUTS(0)
       |      val linkListElementOutput = OUTPUTS(1)
       |
       |      allOf(Coll(
       |        INPUTS(1).tokens(0)._1 == maintainerNFTToken,
       |
       |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
       |
       |        linkListElementOutput.propositionBytes == SELF.propositionBytes,
       |        linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
       |        linkListElementOutput.tokens(0)._2 == 1,
       |        linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
       |        linkListElementOutput.R5[Long].isDefined, // request amount
       |        linkListElementOutput.R6[Long].isDefined, // request id
       |        linkListElementOutput.value == minValue,
       |
       |        OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
       |      ))
       |    }
       |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // approve
       |      allOf(Coll(
       |        INPUTS(2).tokens(0)._1 == linkListNFTToken,
       |        INPUTS(3).propositionBytes == SELF.propositionBytes,
       |        INPUTS(3).id == SELF.id
       |     ))
       |    }
       |    else false
       |  }
       |  sigmaProp (check)
       |}""".stripMargin

  lazy val linkListElementContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("minValue", Configs.defaultTxFee)
      .item("linkListTokenRepoId", ErgoId.create(Configs.luportLinklistRepoTokenId).getBytes)
      .item("maintainerNFTToken", ErgoId.create(Configs.luportMaintainerTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(Configs.luportLinklistTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(Configs.tokenRepoTokenId).getBytes)
      .build(),
    linkListElementScript
  )
  val linkListElementErgoTee: ErgoTree = linkListElementContract.getErgoTree
  val linkListElementHash: Digest32 = scorex.crypto.hash.Blake2b256(linkListElementErgoTee.bytes)

  lazy val linkListRepoContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("linkListTokenRepoId", ErgoId.create(Configs.luportLinklistRepoTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(Configs.luportLinklistTokenId).getBytes)
      .item("maintainerNFTToken", ErgoId.create(Configs.luportMaintainerTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(Configs.tokenRepoTokenId).getBytes)
      .item("minValue", Configs.defaultTxFee)
      .item("linkListElementRepoContractHash", linkListElementHash)
      .build(),
    linkListRepoScript
  )

  lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("maintainerNFTToken", ErgoId.create(Configs.luportMaintainerTokenId).getBytes)
      .item("maintainerRepoId", ErgoId.create(Configs.luportTokenId).getBytes)
      .item("linkListTokenRepoId", ErgoId.create(Configs.luportLinklistRepoTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(Configs.luportLinklistTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(Configs.tokenRepoTokenId).getBytes)
      .item("linkListElementRepoContractHash", linkListElementHash)
      .build(),
    maintainerRepoScript
  )

  linkListAddress = Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString
  maintainerAddress = Configs.addressEncoder.fromProposition(maintainerRepoContract.getErgoTree).get.toString
  linkListElementAddress = Configs.addressEncoder.fromProposition(linkListElementContract.getErgoTree).get.toString
}
