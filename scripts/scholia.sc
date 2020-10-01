// Ammonite script to a create distinct corpus for scholia
val version = "2020i"

val url = s"https://raw.githubusercontent.com/homermultitext/hmt-archive/master/releases-cex/hmt-${version}-texts.cex"


val myBT = coursierapi.MavenRepository.of("https://dl.bintray.com/neelsmith/maven")
interp.repositories() ++= Seq(myBT)
import $ivy.`edu.holycross.shot::ohco2:10.20.4`
import $ivy.`edu.holycross.shot::scm:7.4.0`
import $ivy.`edu.holycross.shot.cite::xcite:4.3.0`


import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.cite._

val lib = CiteLibrarySource.fromUrl(url)
val corpus = lib.textRepository.get.corpus

val scholiaUrn = CtsUrn("urn:cts:greekLit:tlg5026:")
val scholia = corpus.nodes.filter(n => scholiaUrn > n.urn)
val cex = scholia.map(n => n.cex("#")).mkString("\n")

import java.io.PrintWriter
new PrintWriter("scholia-diplomatic.cex"){write(cex);close;}
