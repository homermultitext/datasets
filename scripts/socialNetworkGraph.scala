// Build a graph of cooccurrences of personal names in scholia
// for social network analysis.
//
// Some utility classes to make reference easier:
case class PersonOccurrence(passage: CtsUrn, personUrnString: String) {
  def cex = passage + "#" + personUrnString
}


def quoteIt(s: String): String = "\"" + s + "\""
case class GraphNode(id: String, label: String, weight: Int ) {
  def json : String =  "{ " + quoteIt("id") + ": " + quoteIt(id) + ", " + quoteIt("value") + ": " + weight + ", " + quoteIt("name") + ": " + quoteIt(label) + " }"
}

case class GraphEdge (source: String, target: String, weight: Int) {

    def json:  String  =  "{ " + quoteIt("source") + ": "+ quoteIt(source) + ", " + quoteIt("target") + ": " + quoteIt(target) + ", " + quoteIt("weight") + ": " + weight + " }"
}

case class DirectedGraph(nodes: Vector[GraphNode], edges: Vector[GraphEdge]) {

  def json : String = "{ "  + quoteIt("nodes") + ": [" + nodes.map(_.json).mkString(", ") + "], " + quoteIt("links") + ": ["+ edges.map(_.json).mkString(", ") + "] }"
}


import scala.io.Source
val authList = "https://raw.githubusercontent.com/homermultitext/hmt-authlists/master/data/hmtnames.cex"
import scala.io.Source
val authLines = Source.fromURL(authList).getLines.toVector.tail.tail

val nameIndex = authLines.map(ln => {
  val cols = ln.split("#")
  (cols(0),cols(3))
}).toMap

val indexUrl = "https://raw.githubusercontent.com/homermultitext/datasets/master/data/persons-index.cex"
val pnIndex  = Source.fromURL(indexUrl).getLines.toVector.map (ln => {
  val columns = ln.split("#")
  (columns(0), columns(1))
})


// Nodes in our SNA will be persons.  Persons will have a weight
// representing how many distinct text units they appear in.
val personToPsgMap = pnIndex.groupBy(_._2)
val personWeights = personToPsgMap.toVector.map{ case (s, v) => (s, v.size) }
val graphNodes = personWeights.map( pers =>
  if (nameIndex.keySet.contains(pers._1)) {
    GraphNode(pers._1,nameIndex(pers._1), pers._2)
  } else {
    GraphNode(pers._1,pers._1, pers._2)
  }
)



// Edges in our graph are associations of two persons.  Edges will be
// weighted by how often a pair of names appear in the same text unit.
val psgToPersonMap = pnIndex.groupBy(_._1)
// eliminate multiple appearances of the same name
val singleOccurrences = psgToPersonMap.map{ case (urn, v) => (urn, v.distinct) }
val namesOnly = singleOccurrences.map{ case (urn, v) => v.map {case (u,n) => n }}




// pair with everybody to the right of you:
def edgePairs(
  names: Vector[String],
  pairs: Vector[(String, String)] = Vector.empty[(String,String)]
) : Vector[(String,String)] = {
  if (names.size == 1) {
    pairs
  } else {
    val n1 = names.head
    val newPairs = names.tail.map( n => (n1, n))
    //println("Paired " + n1 + " to " + newPairs.mkString(", "))
    edgePairs(names.tail, pairs ++ newPairs)
  }
}

val paired = namesOnly.map( v => edgePairs(v)).filter(_.nonEmpty)
val edgeData = paired.flatten.groupBy( pr => pr)
val edgeWeights = edgeData.map{ case (pr, v) => (pr, v.size)}


val edges = edgeWeights.map(edge => GraphEdge(edge._1._1, edge._1._2, edge._2 )).toVector


val dag = DirectedGraph(graphNodes, edges)
import java.io.PrintWriter
new PrintWriter("scholia-sna.json"){write(dag.json);close;}
