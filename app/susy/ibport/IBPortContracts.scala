package susy.ibport

import helpers.Configs
import org.ergoplatform.appkit._
import scorex.crypto.hash.Digest32
import sigmastate.Values.ErgoTree

class IBPortContracts(ctx: BlockchainContext) {

  var linkListAddress: String = _
  var maintainerAddress: String = _
  var linkListElementAddress: String = _

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
       |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
       |        linkListTokenOutput.value == INPUTS(0).value - minValue,
       |        blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
       |
       |        OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
       |      ))
       |     }
       |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // ChangeStatus
       |      val linkListTokenOutput = OUTPUTS(1)
       |      allOf(Coll(
       |        linkListTokenOutput.tokens(1)._2 == INPUTS(2).tokens(1)._2 + 1,
       |        linkListTokenOutput.tokens(1)._1 == linkListTokenRepoId,
       |        linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
       |        linkListTokenOutput.propositionBytes == SELF.propositionBytes,
       |        linkListTokenOutput.value == INPUTS(2).value + minValue,
       |
       |        INPUTS(2).propositionBytes == SELF.propositionBytes,
       |        INPUTS(2).id == SELF.id,
       |        blake2b256(INPUTS(3).propositionBytes) == linkListElementRepoContractHash
       |       ))
       |     }
       |    else false
       |  }
       |
       |  sigmaProp (check)
       |}""".stripMargin
  lazy val maintainerRepoScript: String =
    s"""{
       |val storeInMaintainer = {(v: ((Box, Box), (Int, Long) )) => {
       |    if (v._1._1.tokens.size > 1){
       |      allOf(Coll(
       |          v._1._2.value == v._1._1.value,
       |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
       |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 + v._2._2
       |      ))
       |    }
       |    else{
       |       allOf(Coll(
       |          v._1._2.value == v._1._1.value + v._2._2
       |      ))
       |    }
       |  }}
       |
       |val mint: Boolean = {(v: ((Box, Box), (Box, Long))) => {
       |  if (v._1._1.tokens.size > 1){
       |      allOf(Coll(
       |          v._1._2.tokens(1)._1 == v._1._1.tokens(1)._1,
       |          v._1._2.tokens(1)._2 == v._1._1.tokens(1)._2 - v._2._2,
       |          v._1._2.value == v._1._1.value
       |      ))
       |    }
       |    else{
       |      allOf(Coll(
       |          v._1._2.value == v._1._1.value - v._2._2
       |      ))
       |    }
       |  }}
       |
       |val check = {
       |
       |  if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
       |
       |    val linkListTokenOutput = OUTPUTS(0)
       |    val linkListElementOutput = OUTPUTS(1)
       |    val maintainerOutput = OUTPUTS(2)
       |
       |    val fee = INPUTS(1).R4[Int].get
       |    val amount = linkListElementOutput.R5[Long].get
       |
       |    allOf(Coll(
       |      INPUTS(0).tokens(1)._1 == linkListTokenRepoId,
       |      INPUTS(1).propositionBytes == SELF.propositionBytes,
       |      INPUTS(1).id == SELF.id,
       |
       |      linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
       |      blake2b256(linkListElementOutput.propositionBytes) == linkListElementRepoContractHash,
       |
       |      maintainerOutput.tokens(1)._1 == maintainerRepoId,
       |      maintainerOutput.tokens(0)._1 == maintainerNFTToken,
       |      maintainerOutput.propositionBytes == SELF.propositionBytes,
       |      maintainerOutput.R4[Int].get == INPUTS(1).R4[Int].get,
       |      storeInMaintainer(((INPUTS(1), maintainerOutput), (fee, amount))) == true
       |    ))
       |  }
       |  else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // Mint
       |    val maintainerOutput = OUTPUTS(1)
       |    val amount = INPUTS(2).R5[Long].get
       |    val data = INPUTS(0).R5[Coll[Byte]].get
       |    val receiver = data.slice(66, data.size)
       |    allOf(Coll(
       |      INPUTS(2).propositionBytes == SELF.propositionBytes,
       |      INPUTS(2).id == SELF.id,
       |
       |      maintainerOutput.tokens(1)._1 == maintainerRepoId,
       |      maintainerOutput.tokens(0)._1 == maintainerNFTToken,
       |      maintainerOutput.propositionBytes == SELF.propositionBytes,
       |      maintainerOutput.R4[Int].get == INPUTS(2).R4[Int].get,
       |
       |      OUTPUTS(2).tokens(1)._1 == maintainerRepoId,
       |
       |      mint(((INPUTS(2), maintainerOutput), (OUTPUTS(2), amount))) == true,
       |      OUTPUTS(2).propositionBytes == receiver
       |    ))
       |  }
       |  else false
       |}
       |  sigmaProp (check)
       |}""".stripMargin
  lazy val linkListElementScript: String =
    s"""{
       |  val check = {
       |    if (INPUTS(0).tokens(0)._1 == linkListNFTToken){ // create Transfer wrap request
       |      val linkListElementOutput = OUTPUTS(1)
       |      val linkListTokenOutput = OUTPUTS(0)
       |
       |      allOf(Coll(
       |       INPUTS(1).tokens(0)._1 == maintainerNFTToken,
       |
       |       linkListTokenOutput.tokens(0)._1 == linkListNFTToken,
       |
       |       linkListElementOutput.propositionBytes == SELF.propositionBytes,
       |       linkListElementOutput.tokens(0)._1 == linkListTokenRepoId,
       |       linkListElementOutput.tokens(0)._2 == 1,
       |       linkListElementOutput.R4[Coll[Byte]].isDefined, // receiver address
       |       linkListElementOutput.R5[Long].isDefined, // request amount
       |       linkListElementOutput.R6[BigInt].isDefined, // request id
       |       linkListElementOutput.value == minValue,
       |
       |       OUTPUTS(2).tokens(0)._1 == maintainerNFTToken
       |      ))
       |    }
       |    else if (INPUTS(0).tokens(0)._1 == signalTokenNFT){ // ChangeStatus
       |      allOf(Coll(
       |        INPUTS(2).tokens(0)._1 == linkListNFTToken,
       |        INPUTS(3).propositionBytes == SELF.propositionBytes,
       |        INPUTS(3).id == SELF.id
       |      ))
       |     }
       |    else false
       |  }
       |  sigmaProp (check)
       |}""".stripMargin

  lazy val linkListElementContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("minValue", 1000000L)
      .item("linkListTokenRepoId", ErgoId.create(Configs.ibportLinklistRepoTokenId).getBytes)
      .item("maintainerNFTToken", ErgoId.create(Configs.ibportMaintainerTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(Configs.ibportLinklistTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(Configs.tokenRepoTokenId).getBytes)
      .build(),
    linkListElementScript
  )
  val linkListElementErgoTee: ErgoTree = linkListElementContract.getErgoTree
  val linkListElementHash: Digest32 = scorex.crypto.hash.Blake2b256(linkListElementErgoTee.bytes)

  lazy val linkListRepoContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("linkListTokenRepoId", ErgoId.create(Configs.ibportLinklistRepoTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(Configs.ibportLinklistTokenId).getBytes)
      .item("maintainerNFTToken", ErgoId.create(Configs.ibportMaintainerTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(Configs.tokenRepoTokenId).getBytes)
      .item("minValue", 1000000L)
      .item("linkListElementRepoContractHash", linkListElementHash)
      .build(),
    linkListRepoScript
  )

  lazy val maintainerRepoContract: ErgoContract = ctx.compileContract(
    ConstantsBuilder.create()
      .item("maintainerNFTToken", ErgoId.create(Configs.ibportMaintainerTokenId).getBytes)
      .item("maintainerRepoId", ErgoId.create(Configs.ibportGWTokenId).getBytes)
      .item("linkListTokenRepoId", ErgoId.create(Configs.ibportLinklistRepoTokenId).getBytes)
      .item("linkListNFTToken", ErgoId.create(Configs.ibportLinklistTokenId).getBytes)
      .item("signalTokenNFT", ErgoId.create(Configs.tokenRepoTokenId).getBytes)
      .item("linkListElementRepoContractHash", linkListElementHash)
      .build(),
    maintainerRepoScript
  )

  linkListAddress = Configs.addressEncoder.fromProposition(linkListRepoContract.getErgoTree).get.toString
  maintainerAddress = Configs.addressEncoder.fromProposition(maintainerRepoContract.getErgoTree).get.toString
  linkListElementAddress = Configs.addressEncoder.fromProposition(linkListElementContract.getErgoTree).get.toString
}
