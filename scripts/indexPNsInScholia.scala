val releaseId = "2020i"
val myBT = coursierapi.MavenRepository.of("https://dl.bintray.com/neelsmith/maven")
interp.repositories() ++= Seq(myBT)
import $ivy.`edu.holycross.shot::ohco2:10.20.4`
import edu.holycross.shot.ohco2._

val xmlUrl = s"https://raw.githubusercontent.com/homermultitext/hmt-archive/master/releases-cex/hmt-${releaseId}-scholia-xml.cex"
val scholiaXml = CorpusSource.fromUrl(xmlUrl, cexHeader=false)

import scala.xml._
val pairings = for (n <- scholiaXml.nodes) yield {
  val x = XML.loadString(n.text)
  val pns = x \\ "persName"
  pns.toVector.map(pn => {
    val attValue = pn.attributes.asAttrMap.getOrElse("n", "No @n attribute on " + pn.text)
    n.urn -> attValue
  })
}


val authList = "https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtnames.cex"
import scala.io.Source
val authLines = Source.fromURL(authList).getLines.toVector.tail.tail

val nameIndex = authLines.map(ln => {
  val cols = ln.split("#")
  (cols(0),cols(3))
}).toMap


case class GraphNode(id: String, label: String, weight: Int ) {

}
// Nodes in our SNA will be persons.  Persons will have a weight
// representing how many distinct text units they appear in.
val pnIndex = pairings.filter(_.nonEmpty)
val personToPsgMap = pnIndex.flatten.groupBy(_._2)
val personWeights = personToPsgMap.toVector.map{ case (s, v) => (s, v.size) }



// Edges in our graph are associations of two persons.  Edges will be
// weighted by how often a pair of names appear in the same text unit.
val psgToPersonMap = pnIndex.flatten.groupBy(_._1)
// eliminate multiple appearches of the same name
val singleOccurrences = psgToPersonMap.map{ case (urn, v) => (urn, v.distinct) }
val namesOnly = singleOccurrences.map{ case (urn, v) => v.map {case (u,n) => n }}



// Correct for wrong version value in URN
def tidier(person: String): String = {
  person.replaceFirst(".r1:pers", ".v1:pers").replaceFirst("pers:pers", "pers.v1:pers")
}

// pair with everybody to the right of you:
def edgePairs(
  names: Vector[String],
  pairs: Vector[(String, String)] = Vector.empty[(String,String)]
) : Vector[(String,String)] = {
  if (names.size == 1) {
    pairs
  } else {
    val n1 = tidier(names.head)
    val newPairs = names.tail.map( n => (n1, tidier(n)))
    //println("Paired " + n1 + " to " + newPairs.mkString(", "))
    edgePairs(names.tail, pairs ++ newPairs)
  }
}

edgePairs(t1)


val paired = namesOnly.map( v => edgePairs(v)).filter(_.nonEmpty)




case class GraphEdge (source: String, target: String, weight: Int)

val edgeData = paired.flatten.groupBy( pr => pr)
val edgeWeights = edgeData.map{ case (pr, v) => (pr, v.size)}

///////////////////////

val graphNodes = personWeights.map( pers =>
  {
    val tidy = pers._1.replaceFirst(".r1:pers", ".v1:pers").replaceFirst("pers:pers", "pers.v1:pers")
    if (nameIndex.keySet.contains(tidy)) {
    GraphNode(tidy,nameIndex(tidy), pers._2)
  } else {
    GraphNode(tidy,tidy, pers._2)
  }
}
)
val edges = edgeWeights.map(edge => GraphEdge(edge._1._1, edge._1._2, edge._2 )).toVector


edges
case class DirectedGraph(nodes: Vector[GraphNode], edges: Vector[GraphEdge])


val dag = DirectedGraph(graphNodes, edges)
