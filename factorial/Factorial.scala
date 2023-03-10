import scala.annotation.tailrec
import java.math.MathContext

object Factorial{
  
    def recursive(n : Int) : Int =
        if(n==0) 1
            else n * recursive(n-1)
      

    def tailRecursive(n : Int) : Int =
        @tailrec
        def aux(n : Int, acc :Int) :Int =
            if(n == 0) acc
                else aux(n-1, acc * n)
          
        aux(n,1)	   
       

    def tailRecBig(n : BigInt) : BigInt =
        @tailrec
        def aux(n : BigInt, acc :BigInt) :BigInt =   
            if(n == 0) acc
                else aux(n-1, acc * n)
            
        aux(n,1)	   
        
    def functional(n : Int) :Int = (1 to n).reduce(_ * _)
 
    def functionalFoldLeft(n : Int) :Int = (1 to n).foldLeft(1)(_ * _)
     
    def functionalFoldRight(n : Int) :Int = (1 to n).foldRight(1)(_ * _)
     
    def imperative (n : Int) : Int =
        var res = 1
        var x = 1
        while(x <= n)  
            res *= x 
            x += 1
        res

    def imperativeBig (n : BigInt) : BigInt =
        
        var res = BigInt(1)
        var x = BigInt(1)
        
        while(x <= n)  
            res *= x 
            x += 1
        res
    
    def memoized (n:BigInt) : BigInt = ???
       
}

@main
def run() =
    val n = 10
    println("Factorial of 10")
    println("Recursive: " + Factorial.recursive( n))
    println("Tail Recursive: " + Factorial.tailRecursive(  n))
    println("Functional: " + Factorial.functional(n))
    println("Functional Fold Left: " + Factorial.functionalFoldLeft(n))
    println("Functional Fold Right: " + Factorial.functionalFoldRight(n))
    println("Imperative: " + Factorial.imperative(n))
    println("Tail Recursive Big: " + Factorial.tailRecBig(n))
    println("Imperative Big: " + Factorial.imperativeBig(n))