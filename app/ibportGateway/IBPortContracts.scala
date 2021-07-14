package ibportGateway

import helpers.Configs
import org.ergoplatform.appkit._
import scorex.crypto.hash.Digest32
import sigmastate.Values.ErgoTree

object IBPortContracts {
  lazy val linkListTokenId: String = ""
  lazy val linkListRepoTokenId: String = ""
  lazy val maintainerTokenId: String = ""
  lazy val maintainerRepoTokenId: String = ""
  lazy val linkListElementTokenId: String = ""
  lazy val tokenRepoTokenId: String = "fd87d089c2945cfdec4551bc6e846c869ca5ef7e2f7790631fc6afeb708126e5"
}

class IBPortContracts(ctx: BlockchainContext) {

  var linkListAddress: String = _
  var maintainerAddress: String = _
  var linkListElementAddress: String = _
  var tokenRepoAddress: String = _
  var signalAddress: String = _

  /**
   * inputs in CreateTransferWrapRq: 0-> linkListTokenRepo, 1-> maintainerRepo
   * outputs in CreateTransferWrapRq: 0-> linkListTokenRepo, 1-> linkListElementRepo , 2-> maintainerRepo
   *
   * inputs in ChangeStatus: 0-> Signal, 1-> Signal forced input, 2-> linkListTokenRepo, 3-> linkListElementRepo
   * outputs in ChangeStatus: 0-> Signal forced output (tokenRepo), 1-> linkListTokenRepo
   *
   * inputs in Mint: 0-> Signal, 1-> Signal forced input, 2-> maintainerRepo
   * outputs in Mint: 0-> Signal forced output (tokenRepo), 1-> maintainerRepo, 2-> receiver
   */
  lazy val linkListRepoScript =
    s"""{
       |  val check = {
       |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
       |      linkListTokenOutput = OUTPUTS(0)
       |      linkListElementOutput = OUTPUTS(1)
       |      allOf(Coll(
       |        INPUTS(1).tokens(1)._1 == maintainerNFTToken,
       |
       |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
       |        linkListTokenOutput.tokens(0)._2 == INPUTS(0).tokens(0)._2 - 1,
       |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
       |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
       |        linkListTokenOutput.value == INPUTS(0).value - minValue,
       |
       |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
       |
       |        OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
       |     }
       |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // ChangeStatus
       |      linkListTokenOutput = OUTPUTS(1)
       |      allOf(Coll(
       |        linkListTokenOutput.tokens(0)._2 == INPUTS(2).tokens(0)._2 + 1,
       |        linkListTokenOutput.tokens(0)._1 == linkListTokenRepoId,
       |        linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
       |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
       |        linkListTokenOutput.value == INPUTS(2).value + minValue,
       |
       |        INPUTS(2).propositionBytes == SELF.propositionBytes,
       |        blake2b256(INPUTS(3).propositionBytes) == linkListElementRepoContractHash
       |     }
       |    else false
       |  }
       |
       |  sigmaProp (check)
       |}""".stripMargin
  lazy val maintainerRepoScript =
    s"""{
       |val storeInMaintainer: Boolean = {(v: ( (Box, Box), BigInt ) ) => {
       |    if (v._1._1.tokens.size > 1){
       |          v._1._2.value == v._1._1.value &&
       |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1 &&
       |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2 &&
       |    }
       |    else{
       |          v._1._2.value == v._1._1.value + v._2
       |    }
       |  }
       |
       |val mint: Boolean = {(v: ( (Box, Box) , (Box, BigInt) ) ) => {
       |  if (v._1._1.tokens.size > 1){
       |      v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1 &&
       |      v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2 &&
       |      v._1._2.value == v._1._1.value
       |    }
       |    else{
       |       v._1._2.value == v._1._1.value - v._2._2
       |    }
       |  }
       |val check = {
       |
       |  if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
       |    linkListTokenOutput = OUTPUTS(0)
       |    linkListElementOutput = OUTPUTS(1)
       |    maintainerOutput = OUTPUTS(2)
       |    val amount = linkListElementOutput.R5[BigInt].get
       |
       |    allOf(Coll(
       |      INPUTS(0).tokens(0)._1 == linkListTokenRepoId,
       |      INPUTS(1).propositionBytes == SELF.propositionBytes,
       |      INPUTS(1).id == SELF.id,
       |
       |      linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
       |      blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
       |
       |      maintainerOutput.propositionBytes == SELF.propositionBytes,
       |      maintainerOutput.R4[Int].get == INPUTS(0).R4[Int].get,
       |      storeInMaintainer( ( (INPUTS(1), maintainerOutput), amount) ) == true
       |  }
       |  else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // Mint
       |    maintainerOutput = OUTPUTS(1)
       |    val amount = INPUTS(2).R5[BigInt].get
       |    val data = INPUTS(0).R5[Coll[Byte]].get
       |    val receiver = data.slice(66, data.size)
       |    allOf(Coll(
       |      blake2b256(INPUTS(0).propositionBytes) == signalRepoContractHash,
       |      INPUTS(2).propositionBytes == SELF.propositionBytes,
       |      INPUTS(2).id == SELF.id,
       |
       |      maintainerOutput.tokens(0)._1 == maintainerRepoId,
       |      maintainerOutput.tokens(1)._1 == maintainerNFTToken,
       |
       |      mint( (INPUTS(2), maintainerOutput), (OUTPUTS(2), amount) ) == true,
       |      OUTPUTS(2).propositionBytes == receiver
       |  }
       |}
       |  sigmaProp (check)
       |}""".stripMargin
  lazy val linkListElementScript =
    s"""{
       |  val check = {
       |    if (INPUTS(0).tokens(1)._1 == linkListNFTToken){ // create Transfer wrap request
       |      linkListElementOutput = OUTPUTS(1)
       |      linkListTokenOutput = OUTPUTS(0)
       |      allOf(Coll(
       |       INPUTS(1).tokens(1)._1 == maintainerNFTToken,
       |
       |       linkListTokenOutput.tokens(1)._1 == linkListNFTToken,
       |       linkListElementOutput.propositionBytes == SELF.propositionBytes,
       |       linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
       |       linkListElementOutput.tokens(0)._2 == 1,
       |       linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
       |       linkListElementOutput.R5[BigInt].isDefined, // request amount
       |       linkListElementOutput.value == minValue,
       |
       |       OUTPUTS(2).tokens(1)._1 == maintainerNFTToken
       |      ))
       |     }
       |    else if (INPUTS(0).tokens(1)._1 == signalTokenNFT){ // ChangeStatus
       |      allOf(Coll(
       |        INPUTS(2).tokens(1)._1 == linkListNFTToken,
       |        INPUTS(3).propositionBytes == SELF.propositionBytes,
       |        INPUTS(3).id == SELF.id
       |      ))
       |     }
       |    else false
       |  }
       |
       |  sigmaProp (check)
       |}""".stripMargin

  lazy val linkListElementContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("minValue", 1000000L)
      .item("maintainerNFTToken", ErgoId.create(IBPortContracts.maintainerTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(IBPortContracts.linkListTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(IBPortContracts.tokenRepoTokenId).getBytes)
      .item("linkListTokenRepoId", ErgoId.create(IBPortContracts.linkListTokenId).getBytes)
      .build(),
    linkListElementScript
  )
  val linkListElementErgoTee: ErgoTree = linkListElementContract.getErgoTree
  val linkListElementHash: Digest32 = scorex.crypto.hash.Blake2b256(linkListElementErgoTee.bytes)

  lazy val linkListRepoContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("linkListTokenRepoId", ErgoId.create(IBPortContracts.linkListTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(IBPortContracts.linkListTokenId).getBytes)
      .item("maintainerNFTToken", ErgoId.create(IBPortContracts.maintainerTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(IBPortContracts.tokenRepoTokenId).getBytes)
      .item("minValue", 1000000L)
      .item("linkListElementRepoContractHash", linkListElementHash)
      .build(),
    linkListRepoScript
  )

  lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("maintainerNFTToken", ErgoId.create(IBPortContracts.maintainerTokenId).getBytes)
      .item("maintainerRepoId", ErgoId.create(IBPortContracts.maintainerRepoTokenId).getBytes)
      .item("linkListTokenRepoId", ErgoId.create(IBPortContracts.linkListRepoTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(IBPortContracts.linkListTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(IBPortContracts.tokenRepoTokenId).getBytes)
      .item("linkListElementRepoContractHash", linkListElementHash)
      .build(),
    maintainerRepoScript
  )

  linkListAddress = Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString
  maintainerAddress = Configs.addressEncoder.fromProposition(maintainerRepoContract.getErgoTree).get.toString
  linkListElementAddress = Configs.addressEncoder.fromProposition(linkListElementContract.getErgoTree).get.toString
}
