package recfun

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
    	if (c == 0 || c == r) 
	    return 1
	else 
	    return pascal(c - 1, r - 1) + pascal (c, r -1) 
    }



  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
	def process(chars: List[Char], counter: Int): Boolean = (chars, counter) match {
	    case (Nil, cnt) => cnt == 0
	    case ('(' :: xs, cnt) => process(xs, cnt + 1)
	    case (')' :: xs, cnt) => cnt > 0 && process(xs, cnt - 1) 
	    case (_ :: xs, cnt) => process(xs, cnt) 
        }
        process(chars,0)
	
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
	if (money == 0)
	    return 1
	else if (money > 0 && !coins.isEmpty)
	    return countChange( money - coins.head, coins) + countChange(money, coins.tail)
	else 
	    return 0 

  }
}
