package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceInternal(currOpenParantheses: Int, remainingChars: List[Char]): Boolean = {
      if (remainingChars.isEmpty) {
        currOpenParantheses == 0
      } else {
        val currChar: Char = remainingChars.head
        val currRemaining: List[Char] = remainingChars.tail
        if (currChar == ')') {
          if (currOpenParantheses == 0) {
            false
          } else {
            balanceInternal(currOpenParantheses - 1, currRemaining)
          }
        } else if (currChar == '(') {
          balanceInternal(currOpenParantheses + 1, currRemaining)
        } else {
          balanceInternal(currOpenParantheses, currRemaining)
        }
      }
    }
    balanceInternal(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) {
      0
    } else if (money < 0) {
      0
    } else if (money == 0) {
      1
    } else {
      countChange(money - coins.head, coins) +
        countChange(money, coins.tail)
    }
  }
}
