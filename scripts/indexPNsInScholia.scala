// Ammonite script to read XML of scholia and create a simple
// two-column index of personal name identifiers and the passage
// where they occur.

val myBT = coursierapi.MavenRepository.of("https://dl.bintray.com/neelsmith/maven")
interp.repositories() ++= Seq(myBT)
import $ivy.`edu.holycross.shot::ohco2:10.20.4`
import $ivy.`edu.holycross.shot.cite::xcite:4.3.0`

import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._

val xmlUrl = s"https://raw.githubusercontent.com/homermultitext/datasets/master/data/hmt-scholia-xml.cex"
val scholiaXml = CorpusSource.fromUrl(xmlUrl, cexHeader=false)


case class PersonOccurrence(passage: CtsUrn, personUrnString: String) {
  def cex = passage + "#" + personUrnString.replaceFirst("pers:pers", "pers.v1:pers").replaceFirst(".r1:pers", ".v1:pers")
}


import scala.xml._
val pairings = for (n <- scholiaXml.nodes) yield {
  val x = XML.loadString(n.text)
  val pns = x \\ "persName"
  pns.toVector.map(pn => {
    val attValue = pn.attributes.asAttrMap.getOrElse("n", "No @n attribute on " + pn.text)
    PersonOccurrence(n.urn, attValue)
  })
}



val cex = pairings.filter(_.nonEmpty).flatten.map(pers => pers.cex).mkString("\n")

import java.io.PrintWriter
new PrintWriter("persons-index.cex"){ write(cex);close;}
