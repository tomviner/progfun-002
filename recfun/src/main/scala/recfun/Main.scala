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
    (c, r) match {
      case (0, 0) => 1
      case (c, r) if 0<=c && c<=r =>
        pascal(c-1, r-1) + pascal(c, r-1)
      case _ => 0
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def count(n: Int, chs: List[Char]): Boolean = {
      if (chs.isEmpty) n==0
      else if (n < 0) false
      else count(
          n + (chs.head match {
            case '(' => 1
            case ')' => -1
            case _ => 0
          }), chs.tail)
    }
    count(0, chars)
  }

  /**
   * Exercise 3
   */
  var j=0
  def myCountChange(money: Int, coins: List[Int]): Int = {
    println("myCountChange")
/*
  count(ways)
  count = (count(ways starting with x) for x in coins).sum
  count = (coins.map(x => count(x))).sum
*/
    def count(mon: Int, cns: List[Int], d: Int): Int = {
      j = j + 1
      if (mon==0) 1
      else if (mon<0) 0
      else {
        cns.map((c: Int) =>
          println(List('"', d, ":ch(", mon, ",", cns, ")\"", "--",
                  '"', d+1, ":ch(", mon-c, ",", coins.filter(_<=c), ")\"", ";").mkString(""))
        )
        cns.map(c => count(mon-c, coins.filter(_<=c), d+1)).sum
      }
    }
    count(money, coins, 1)
  }

  var i=0
  def countChange(money: Int, coins: List[Int]): Int = {
    println("countChange")
    def count(mon: Int, cns: List[Int], d: Int): Int = {
      i = i + 1
      if (mon==0) 1
      else if (mon<0 || cns.isEmpty) 0
      else {
        println(List('"', d, ":ch(", mon, ",", cns, ")\"", "--",
                '"', d+1, ":ch(", mon-cns.head, ",", cns, ")\"", ";").mkString(""))
        println(List('"', d, ":ch(", mon, ",", cns, ")\"", "--",
                '"', d+1, ":ch(", mon, ",", cns.tail, ")\"", ";").mkString(""))
        count(mon-cns.head, cns, d+1) + count(mon, cns.tail, d+1)
      }
    }
    count(money, coins, 1)
  }
}
