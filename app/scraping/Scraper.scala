package scraping

trait Scraper[T] {
  scr =>
  def loadURL(url: String): T

  def loadString(s: String): T

  def map[S](f: T => S): Scraper[S] = {
    new Scraper[S] {
      def loadString(s: String) = f(scr.loadString(s))

      def loadURL(url: String) = f(scr.loadURL(url))
    }
  }
}