// Ammonite script
val version = "2020i"

val url = s"https://raw.githubusercontent.com/homermultitext/hmt-archive/master/releases-cex/hmt-${version}.cex"


val myBT = coursierapi.MavenRepository.of("https://dl.bintray.com/neelsmith/maven")
interp.repositories() ++= Seq(myBT)
import $ivy.`edu.holycross.shot::scm:7.4.0`
import $ivy.`edu.holycross.shot::ohco2:10.20.4`
import $ivy.`edu.holycross.shot.cite::xcite:4.3.0`
import $ivy.`edu.holycross.shot::dse:7.1.3`

import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.cite._
import edu.holycross.shot.dse._

val lib = CiteLibrarySource.fromUrl(url)
val relations = lib.relationSet.get


// subset texts and relations for scholia only:
val scholiaUrn = CtsUrn("urn:cts:greekLit:tlg5026:")
def scholia(citeLib : CiteLibrary): Corpus = {
  val corpus = citeLib.textRepository.get.corpus
  val scholia = corpus.nodes.filter(n => scholiaUrn > n.urn)
  Corpus(scholia)
}
val corpus = scholia(lib)
val canonicalUrns = corpus.nodes.map(n => n.urn.collapsePassageBy(1)).distinct

val commentVerb = Cite2Urn("urn:cite2:cite:verbs.v1:commentsOn")
val comments = relations.relations.toVector.filter(_.relation == commentVerb)

val indexOption = for (u <- canonicalUrns) yield {
    val matched = comments.filter(_.urn1 == u)
    if (matched.nonEmpty) {
      (u, Some(matched.head))
    } else {
      (u, None)
    }
}

val badIndex = indexOption.filter(_._2 == None).map(_._1)
val goodPairs = indexOption.filter(_._2 != None).map(pr => pr._1 + "#" + pr._2)


indexOption.size
badIndex.size
import java.io.PrintWriter

new PrintWriter("bad-scholion-iliad-inde.txt"){write(badIndex.mkString("\n"));close;}

new PrintWriter("scholion-iliad-index.cex"){write(goodPairs.mkString("\n"));close;}
