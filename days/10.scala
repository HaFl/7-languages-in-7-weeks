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

// my first stab - that must be quite ugly to look at for a seasoned
// Scala programmer

object TicTacToeBoard {
  
  var board = Array.fill[String](3, 3) { " " }
  
  val players = List(0, 1)
  var currentPlayer = 0
  def nextPlayer() {
    currentPlayer = (currentPlayer + 1) % players.size
  }
  
  def add(player: String, x: Int, y: Int) {
    board(3 - y)(x - 1) = player
  }
  
  def isEmpty(x: Int, y: Int) = (board(3 - y)(x - 1) == " ")
  
  override def toString = {
    var res = ""
    for(i <- 0 until board.length) {
      for(j <- 0 until board(i).length) {
        res += board(i)(j)
        if(j != board(i).length - 1) {
          res += "|"
        }
      }
      res += "\n"
      if(i != board.length - 1) {
        for(j <- 0 until board(i).length * 2 - 1) {
          res += "_"
        }
        res += "\n"
      }
    }
    res
  }
  
  def gameOver(board: Array[Array[String]]): Boolean = {
    // columns init
    val firstColChars = board(0)
    var gameOverCols = Array(true, true, true)
    
    // diagonals init
    val firstDiagChars = Array(board(0)(0), board(0)(2))
    var gameOverDiags = Array(true, true)
    
    for(row <- 0 until board.length) {
      
      // row init
      var gameOverRow = true
      val firstRowChar = board(row)(0)
      
      for(col <- 0 until board(row).length) {
        if(board(row)(col) == " ") {
          gameOverRow = false
          gameOverCols(col) = false
        } else {
          if(board(row)(col) != firstRowChar) {
            gameOverRow = false
          }
          if(board(col)(row) != firstColChars(col)) {
            gameOverCols(col) = false
          }
        }
        // check diagonals
        if (row == col && (board(row)(col) == " " || board(row)(col) != firstDiagChars(0))) {
          gameOverDiags(0) = false
        }
        if (row == (2 - col) && (board(row)(col) == " " || board(row)(col) != firstDiagChars(1))) {
          gameOverDiags(1) = false
        }
      }
      if(gameOverRow) {
        return true
      }
    }
    for(col <- gameOverCols) {
      if(col) return true
    }
    for(diag <- gameOverDiags) {
      if(diag) return true
    }
    false
  }
}

print("Player 1 - please insert your name: ")
val player1 = readLine()
print("Player 2 - please insert your name: ")
val player2 = readLine()
val playerNames = Array[String](player1, player2)
val playerChars = Array[String]("X", "O")
println("Alright, let's do this! We start from here...")
println(TicTacToeBoard)
while (!TicTacToeBoard.gameOver(TicTacToeBoard.board)) {
    print(playerNames(TicTacToeBoard.players(TicTacToeBoard.currentPlayer)) + " - what's your next move (format: X,Y): ")
    val move = readLine()
    val axes = move.split(",")
    // while isEmpty and error handling is missing
    TicTacToeBoard.add(playerChars(TicTacToeBoard.players(TicTacToeBoard.currentPlayer)), axes(0).toInt, axes(1).toInt)
    println(TicTacToeBoard)
    TicTacToeBoard.nextPlayer
}
TicTacToeBoard.nextPlayer
println("And the winner is: " + playerNames(TicTacToeBoard.currentPlayer))
