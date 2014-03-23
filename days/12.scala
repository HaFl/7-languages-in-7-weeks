#!/usr/bin/env scala

// FIND, DO 1., 2.
import scala.io._
import scala.actors._
import Actor._

object PageLoader {

	// could be more sophisticated of course
	val linkRegex = "<a href=\"(http://.*?)\"".r

	def getPage(url: String) = {
		try {
			Source.fromURL(url).mkString
		} catch {
			case _: Throwable => System.err.println("Error for: " + url); ""
		}
	}

	def getPageSize(url : String) = getPage(url).length

	def getLinkNumber(url: String) = linkRegex.findAllMatchIn(getPage(url)).length

	def getPageSizeDepth2(url: String) = {
		val links = linkRegex.findAllMatchIn(getPage(url)).foldLeft(Set(url))(
			(allLinks, m) => allLinks + m.group(1))
		links.foldLeft(0)((sum, link) => sum + getPageSize(link))
	}
}

val urls = List(
	"http://www.bild.de/",
	"http://florianhartl.com/",
	"http://www.techcrunch.com/",
	"http://www.cnn.com/")

def timeMethod(f: () => Unit) = {
	val start = System.nanoTime
	f()
	val end = System.nanoTime
	println("Method took " + (end - start)/1000000000.0 + " seconds.")
}

def getPageSequentially(pageLoaderMethod: String => Int)() = {
	for(url <- urls) {
		println("Size for " + url + ": " + pageLoaderMethod(url))
	}
}

def getPageConcurrently(pageLoaderMethod: String => Int)() = {
	val caller = self
	for(url <- urls) {
		actor { caller ! (url, pageLoaderMethod(url)) }
	}
	for(i <- 1 to urls.size) {
		receive {
			case (url, size) => println("Size for " + url + ": " + size)
		}
	}
}

/** So what happens? Well, obviously there is only one actor which has to do
	* all the work. As this means that there is no concurrency involved, such an
	* approach is equivalent to the sequential one. Therefore, the perfomance
	* degrades and should actually be similar to the sequential run.
	*
	* On my machine the concurrent run with one actor is still a lot faster than
	* the sequential one. However, this is due to caching or some other sort of
	* optimization that can be made use of since we run it after the sequential
	* approach and basically do the same work. Actually, the concurrent one actor
	* might even be slower due to some "actor" overhead.
	*/
def getPageConcurrentlyOneActor(pageLoaderMethod: String => Int)() = {
	val caller = self
	actor {
		for(url <- urls) {
			caller ! (url, pageLoaderMethod(url))
		}
	}
	for(i <- 1 to urls.size) {
		receive {
			case (url, size) => println("Size for " + url + ": " + size)
		}
	}
}

println("--> Get size of web page")
println("Sequential run:")
timeMethod(getPageSequentially(PageLoader.getPageSize))
println("Concurrent run (one actor):")
timeMethod(getPageConcurrentlyOneActor(PageLoader.getPageSize))
println("Concurrent run:")
timeMethod(getPageConcurrently(PageLoader.getPageSize))

println("--> Get number of links of web page")
println("Sequential run:")
timeMethod(getPageSequentially(PageLoader.getLinkNumber))
println("Concurrent run (one actor):")
timeMethod(getPageConcurrentlyOneActor(PageLoader.getLinkNumber))
println("Concurrent run:")
timeMethod(getPageConcurrently(PageLoader.getLinkNumber))

println("--> Get size of website with depth 2")
println("Sequential run:")
timeMethod(getPageSequentially(PageLoader.getPageSizeDepth2))
// Would be even faster if with the actors we would again create an actor for
// each link
println("Concurrent run:")
timeMethod(getPageConcurrently(PageLoader.getPageSizeDepth2))

// interesting wrap up to scala can be found on
// http://brikis98.blogspot.de/2012/04/seven-languages-in-seven-weeks-scala.html
