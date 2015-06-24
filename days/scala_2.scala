#!/usr/bin/env scala

/* FIND

- How to use Scala files:
	http://www.tutorialspoint.com/scala/scala_file_io.htm
	(especially how to read a file is interesting)

- closure vs. code block:
	A closure is an anonymous function (= code block) which has a variable
	that is not defined in the function (hence, it must have been declared
	before the function is defined). Java lacks such a functionality.

	Example:
	var x = 0
	val fun = ()=>{x=1}
	fun
*/


// DO

// 1.
val list = List("This", "is", "a", "list", "of", "Strings", "!")
println(list.foldLeft(0)((sum, value) => sum + 1))
println(list.foldLeft(0)((sum, value) => sum + value.length))

// 2., 3.
import scala.io.Source
import scala.collection.mutable.HashMap

trait Censor {
	def getCurseWords(path: String="../data/curseWords.txt") = {
		val curseWords = new HashMap[String, String]
		Source.fromFile(path).getLines().foreach{ line =>
			val words = line.split("\t")
			curseWords += words(0) -> words(1)
		}
		curseWords
	}

	def censor(text: String, path: Option[String] = None) = {
		val curseWords = if (path.isDefined) getCurseWords(path.get) else getCurseWords()
        curseWords.foldLeft(text)((replacedText, badWord) => replacedText.replaceAll(badWord._1, badWord._2))
	}
}

object Test extends Censor
//val test = new Test with Censor
val text = "Oh my god, this is a bad curse, ohno."
println(Test.censor(text))
