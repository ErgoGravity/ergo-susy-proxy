package helpers

import java.math.BigInteger

import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.{Address, NetworkType, RestApiErgoClient}

object Configs extends ConfigHelper {
  lazy val nodeUrl: String = readKey("node.url")
  lazy val nodeApiKey: String = readKey("node.apiKey", "")
  lazy val proxySecret: BigInteger = BigInt(readKey("proxy.secret"), 16).bigInteger
  lazy val proxyAddress: Address = Address.create(readKey("proxy.address"))
  lazy val networkType: NetworkType = if (readKey("node.networkType").toLowerCase.equals("mainnet")) NetworkType.MAINNET else NetworkType.TESTNET
  lazy val addressEncoder = new ErgoAddressEncoder(networkType.networkPrefix)

  private lazy val explorerUrlConf = readKey("explorer.url", "")
  lazy val explorerUrl: String = if (explorerUrlConf.isEmpty) RestApiErgoClient.getDefaultExplorerUrl(Configs.networkType) else explorerUrlConf
  lazy val signalBoxValue: Long =  1000000L
  lazy val defaultTxFee: Long =  1000000L

  lazy val swTokenId: String = readKey("tokens.luport.swTokenId")
  lazy val luportMaintainerTokenId: String = readKey("tokens.luport.maintainerTokenId")
  lazy val luportLinklistTokenId: String = readKey("tokens.luport.linklistTokenId")
  lazy val luportLinklistRepoTokenId: String = readKey("tokens.luport.linklistRepoTokenId")

  lazy val gwswTokenId: String = readKey("tokens.ibport.gravityTokenId")
  lazy val ibportMaintainerTokenId: String = readKey("tokens.ibport.oracleTokenId")
  lazy val ibportLinklistTokenId: String = readKey("tokens.ibport.pulseTokenId")
  lazy val ibportLinklistRepoTokenId: String = readKey("tokens.ibport.tokenRepoTokenId")

  lazy val tokenRepoTokenId: String = readKey("tokens.tokenRepoTokenId")
  lazy val signalAddress: String = readKey("contracts.signalAddress")
  lazy val tokenRepoAddress: String = readKey("contracts.tokenRepoAddress")
}
