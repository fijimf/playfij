package scraping

import xml.Node
import org.xml.sax.InputSource
import java.io.{InputStream, Reader, StringReader}
import scala.util.control.Exception._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import xml.parsing.NoBindingFactoryAdapter
import java.net.URL


object HtmlHelper {
  def loadHtml(url: String): Option[Node] = {
    catching(classOf[Exception]).opt {
      val adapter = new NoBindingFactoryAdapter()
      adapter.loadXML(new InputSource(url), new SAXFactoryImpl().newSAXParser())
    }
  }

  def loadHtmlFromReader(r: Reader): Option[Node] = {
    catching(classOf[Exception]).opt {
      val adapter = new NoBindingFactoryAdapter()
      adapter.loadXML(new InputSource(r), new SAXFactoryImpl().newSAXParser())
    }
  }

  def loadHtmlFromString(s: String): Option[Node] = {
    loadHtmlFromReader(new StringReader(s))
  }
}

trait HttpScraper extends Scraper[Node] {

  def loadURL(u: String): Node = {

    val url:URL = new URL(u);
    val stream:InputStream = url.openStream();
    val source: InputSource = new org.xml.sax.InputSource(stream)
    loadPage(source)
  }

  def loadString(s: String): Node = {
    loadReader(new StringReader(s))
  }

  def loadReader(reader: Reader): Node = {
    loadPage(new InputSource(reader))
  }

  def loadPage(source: InputSource): Node = {
    try {
      val parserFactory = new org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
      val parser = parserFactory.newSAXParser()
      val adapter = new scala.xml.parsing.NoBindingFactoryAdapter
      adapter.loadXML(source, parser)
    }
    catch {
      case t: Throwable => <exception>
        {t.getMessage}
      </exception>
    }
  }
}
