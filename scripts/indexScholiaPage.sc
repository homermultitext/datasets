// Ammonite script to a create distinct corpus for scholia
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
val libDse = DseVector.fromCiteLibrary(lib)

// subset texts and DSE records for scholia only:
val scholiaUrn = CtsUrn("urn:cts:greekLit:tlg5026:")

def scholia(citeLib : CiteLibrary): Corpus = {
  val corpus = citeLib.textRepository.get.corpus
  val scholia = corpus.nodes.filter(n => scholiaUrn > n.urn)
  Corpus(scholia)
}

def scholiaDse(dseVector: DseVector) = {
  DseVector(dseVector.passages.filter(psg => scholiaUrn > psg.passage))
}

val corpus = scholia(lib)
val dse = scholiaDse(libDse)

val canonicalUrns = corpus.nodes.map(n => n.urn.collapsePassageBy(1)).distinct
canonicalUrns.size

val indexPair = canonicalUrns.map(u => (u, dse.tbsForText(u)))

val badIndex = indexPair.filter(_._2 == None).map(_._1)
val goodPairs = indexPair.filter(_._2 != None).map(pr => pr._1 + "#" + pr._2)

import java.io.PrintWriter

new PrintWriter("bad-index.txt"){write(badIndex.mkString("\n"));close;}

new PrintWriter("scholia-index.cex"){write(goodPairs.mkString("\n"));close;}
