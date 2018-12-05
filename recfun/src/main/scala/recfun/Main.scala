package recfun

import scala.annotation.tailrec

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
    def isEdge(c: Int, r: Int) = r == 0 || c == 0 || c == r

    if (isEdge(c, r)) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balanceIfStatement(chars: List[Char]): Boolean = {
    @tailrec
    def balanceTail(nestLevel: Int, chars: List[Char]): Boolean =
      if (nestLevel < 0) false
      else if (chars.isEmpty) nestLevel == 0
      else if (chars.head == '(') balanceTail(nestLevel + 1, chars.tail)
      else if (chars.head == ')') balanceTail(nestLevel - 1, chars.tail)
      else balanceTail(nestLevel, chars.tail)

    balanceTail(0, chars)
  }

  def balance(chars: List[Char]): Boolean = {

    @tailrec
    def balanceTail(nestLevel: Int, chars: List[Char]): Boolean =
      if (nestLevel < 0) false
      else chars match {
        case Nil => nestLevel == 0
        case '(' :: tail => balanceTail(nestLevel + 1, tail)
        case ')' :: tail => balanceTail(nestLevel - 1, tail)
        case _ => balanceTail(nestLevel, chars.tail)
      }

    balanceTail(0, chars)
  }

  /**
    * Exercise 3
    *
    * money = 5
    * coins = 1 2
    *
    * take first coin = 1
    * +1 if money can be changed with just 1
    * + count if money can be changed with multiplies of 1st coin and any combination of other coins
    * + count money can be changed with other coins without first coin
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeForThatCoinOnly(coin: Int, money: Int): Int = if (money % coin == 0) 1 else 0

    if (money <= 0 || coins.isEmpty) 0
    else {
      val thatCoin = coins.head
      val withoutThatCoin = coins.tail
      val countsForRepeatedCoin = for(remainingMoney <- (money - thatCoin) to 0 by (-thatCoin)) yield countChange(remainingMoney, withoutThatCoin)

      countChangeForThatCoinOnly(thatCoin, money) + countsForRepeatedCoin.sum + countChange(money, withoutThatCoin)
    }
  }

}
