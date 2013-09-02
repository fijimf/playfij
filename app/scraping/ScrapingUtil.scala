package scraping

object ScrapingUtil {
  def textKey(name: String): String = {
    name.replaceAll("[']", "").replaceAll("[^\\-a-zA-Z0-9 ]", " ").trim.toLowerCase.replaceAll(" +", "-")
  }

  def nameToKey(n: String): String = {
    textKey(n.replaceFirst(" Conference$", "").replaceFirst(" League$", "").replaceFirst("^The ", ""))
  }

}
