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
    
    if(c==0||c==r)
      1
    else 
      (factoriel(r)/(factoriel(c)*factoriel(r-c)))
  }

  def factoriel(x:Int):Int={ 
    if(x==1)1 else (factoriel(x-1)*x)
  }
  /**
   * Exercise 2
   */
  
 def balance(chars: List[Char]): Boolean = {
    def checkParenthesis(chars:List[Char],i:Int):Boolean = {
     if(i<0||(chars.isEmpty&&i!=0))
       false
       else if(chars.isEmpty&&i==0)
         true
     else {       
      if(chars.head.equals('('))        
     checkParenthesis(chars.tail,(i+1))
      else if(chars.head.equals(')'))
            checkParenthesis(chars.tail,(i-1))            
       else
           checkParenthesis(chars.tail,i) 
     }
  }
    if(chars.isEmpty) false
    else
    checkParenthesis(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {   
    if(money==0)
      1
    else if(money<0) 0
    else if(coins.isEmpty)
      0
    else{
     countChange(money - coins.head, coins) +countChange(money, coins.tail)
    }
  }

}
