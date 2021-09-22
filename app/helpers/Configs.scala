package helpers

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType, RestApiErgoClient}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val proxySecret: BigInteger = BigInt(readKey("proxy.secret"), 16).bigInteger
  lazy val proxyAddress: Address = Address.create(readKey("proxy.address"))
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)

  private lazy val explorerUrlConf = readKey("explorer.url", "")
  lazy val explorerUrl: String = if (explorerUrlConf.isEmpty) RestApiErgoClient.getDefaultExplorerUrl(Configs.networkType) else explorerUrlConf
  lazy val signalBoxValue: Long =  readKey("signalBoxValue").toLong
  lazy val defaultTxFee: Long =  readKey("defaultTxFee").toLong

  lazy val luportTokenId: String = readKey("tokens.luport.tokenId")
  lazy val luportMaintainerTokenId: String = readKey("tokens.luport.maintainerTokenId")
  lazy val luportLinklistTokenId: String = readKey("tokens.luport.linkListTokenId")
  lazy val luportLinklistRepoTokenId: String = readKey("tokens.luport.linkListRepoTokenId")

  lazy val ibportGWTokenId: String = readKey("tokens.ibport.gwTokenId")
  lazy val ibportMaintainerTokenId: String = readKey("tokens.ibport.maintainerTokenId")
  lazy val ibportLinklistTokenId: String = readKey("tokens.ibport.linkListTokenId")
  lazy val ibportLinklistRepoTokenId: String = readKey("tokens.ibport.linkListRepoTokenId")

  lazy val tokenRepoTokenId: String = readKey("tokens.tokenRepoTokenId")
  lazy val oracleTokenId: String = readKey("tokens.oracleTokenId")
  lazy val signalAddress: String = readKey("contracts.signalAddress")
  lazy val tokenRepoAddress: String = readKey("contracts.tokenRepoAddress")
  lazy val oracleAddress: String = readKey("contracts.oracleAddress")
}
