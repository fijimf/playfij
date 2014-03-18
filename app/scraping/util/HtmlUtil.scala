package scraping.util

import xml.Node
import org.xml.sax.InputSource
import java.io.{InputStream, Reader, StringReader}
import scala.util.control.Exception._
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import xml.parsing.NoBindingFactoryAdapter
import java.net.URL


object HtmlUtil {

  def loadHtmlFromReader(r: Reader): Option[Node] = {
    catching(classOf[Exception]).opt {
      new NoBindingFactoryAdapter().loadXML(new InputSource(r), new SAXFactoryImpl().newSAXParser())
    }
  }

  def loadHtmlFromString(s: String): Option[Node] =  loadHtmlFromReader(new StringReader(s)) 

  def attrValue(n: Node, attr: String): Option[String] = {
    n.attribute(attr).flatMap(_.headOption).map(_.text)
  }

  def attrMatch(n: Node, attr: String, value: String): Boolean = {
    n.attribute(attr) match {
      case Some(nodeStr) => nodeStr.exists(_.text == value)
      case _ => false
    }
  }
}