#!/usr/bin/env scala

// from tutorial of day 1 with scala
def whileLoop {
    var i = 1
    while(i <= 3) {
        println(i)
        i += 1
    }
}
whileLoop

def forLoop {
    for(i <- 0 until args.length) {
        println(args(i))
    }
}
forLoop

def rubyStyleForLoop {
    println("for loop using Ruby-style iteration")
    args.foreach { arg =>
        println(arg)
    }
}
rubyStyleForLoop

// basic class example
class Compass {

    val directions = List("north", "east", "south", "west")
    var bearing = 0

    print("Initial bearing: ")
    println(direction)

    def direction() = directions(bearing)

    def inform(turnDirection: String) {
        println("Turning " + turnDirection + ". Now bearing " + direction)
    }

    def turnRight() {
        bearing = (bearing + 1) % directions.size
        inform("right" )
    }

    def turnLeft() {
        bearing = (bearing + (directions.size - 1)) % directions.size
        inform("left" )
    }
}
val myCompass = new Compass
myCompass.turnRight
myCompass.turnRight
myCompass.turnLeft
myCompass.turnLeft
myCompass.turnLeft

// alternate constructor
class Person(first_name: String) {
    println("Outer constructor")

    def this(first_name: String, last_name: String) {
        this(first_name)
        println("Inner constructor")
    }

    def talk() = println("Hi")
}
val bob = new Person("Bob")
val bobTate = new Person("Bob", "Tate")

/* other code samples in the book for: 
 * objects (class methods), inheritance, traits
 */


/* FIND
 *
 * scala API: http://www.scala-lang.org/api/2.10.3/#package
 *
 * scala vs java:
 *    just read many comparisons: overall Scala introduces functional programming
 *    while trying to stick to the roots of Java. Hence it is not as bloated
 *    as Java and can be way more elegant but all this comes at the price
 *    of complexity and - if misused - readability
 *
 * val vs var:
 *    - val means immutable, var means mutable
 *    - val leads to safer (threads etc.) and more readable (reuse of vars) code
 *    - var can improve performance a lot in some situations
 *    => both are useful and one should try to use val as much as possible
 */


// DO - tic tac toe


