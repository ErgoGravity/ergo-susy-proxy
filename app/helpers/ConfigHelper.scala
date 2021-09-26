package helpers

import com.typesafe.config.ConfigFactory
import network.GetRequest.getDetails
import play.api.{Configuration, Logger}

trait ConfigHelper {

  val config: Configuration = Configuration(ConfigFactory.load())
  private val logger: Logger = Logger(this.getClass)

  /**
   * Read the config and return the value of the key
   *
   * @param key     key to find
   * @param default default value if the key is not found
   * @return value of the key
   */
  def readKey(key: String, default: String = null): String = {
    try {
      if (default != null && !config.has(key)) return default
      if (config.has(key) && config.getOptional[String](key).getOrElse(default).nonEmpty) config.getOptional[String](key).getOrElse(default)
      else throw config.reportError(key, s"$key is required.")
    } catch {
      case ex: Throwable =>
        logger.error(ex.getMessage)
        sys.exit()
    }
  }

  def readNodes(): Seq[String] = {
    try {
      val key = "nodes"
      if (config.has(key)) config.getOptional[Seq[String]](key).getOrElse(Seq()).map(ip => {
        if (!ip.startsWith("http")) "http://" + ip
        else ip
      })
      else throw config.reportError(key, s"$key is required.")
    } catch {
      case ex: Throwable =>
        logger.error(ex.getMessage)
        sys.exit()
    }
  }

  def getTokens(key: String, default: String = null): Map[String, String]= {
    try {
      val url = readKey(key)
      val tokens = gatewayTokens(url)
      tokens
    } catch {
      case ex: Throwable =>
        logger.error(ex.getMessage)
        sys.exit()
    }
  }
  def getContracts(key: String, default: String = null): Map[String, String]= {
    try {
      val url = readKey(key)
      val Contracts = gatewayContracts(url)
      Contracts
    } catch {
      case ex: Throwable =>
        logger.error(ex.getMessage)
        sys.exit()
    }
  }

  def gatewayTokens(url: String): Map[String, String] = {
    val data = getDetails(url)
    val tokenIds = data.hcursor.downField("tokenIds").as[Map[String, String]].getOrElse(throw new Throwable("parse error"))
    tokenIds
  }
  def gatewayContracts(url: String): Map[String, String] = {
    val data = getDetails(url)
    val contractAddreses = data.hcursor.downField("contractAddreses").as[Map[String, String]].getOrElse(throw new Throwable("parse error"))
    contractAddreses
  }
}
