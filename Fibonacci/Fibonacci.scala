import java.math.MathContext
import math.BigDecimal.javaBigDecimal2bigDecimal
import scala.annotation.tailrec

object Root {

    def sqrt(n : Int) : BigDecimal = {
  
        val root = JRoot.bigSqrt(new java.math.BigDecimal(n)) // JRoot implementation of good precision root
     
        return BigDecimal(root)
    }
}

object FibClosedForm {

    /* Closed Form Algorithm	
    * Fib(n) = 1/sqrt(5) * (phi^n - psi'^n)
    phi = (1 + sqrt(5) / 2)
    psi = (1 - sqrt(5) / 2)
    * 
    *   
    */
    def int (n : Int) : Int = {

        val root5 = math.sqrt(5)

        val phi = (1 + root5)/2

        val psi = (1- root5)/2

        val fn = (math.pow(phi,n) -  math.pow(psi,n))/root5

        fn.toInt
    }

    def long (n : Int) : Long = {

        val root5 = math.sqrt(5)

        val phi : Double = (1 + root5)/2

        val psi : Double  = (1- root5)/2

        val fn : Double = (math.pow(phi,n) -  math.pow(psi,n))/root5

        fn.toLong
    }

    def bigInt (n : Int) : BigInt = {

        val root5 = Root.sqrt(5)

        val phi  = (1 + root5)/2

        val psi  = (1 - root5)/2

        val fn : BigDecimal = (phi.pow(n) -  psi.pow(n))/root5

        fn.toBigInt
    }
}

object Fibonacci :

    def tailRecursive(n :Int) : Int =
        @tailrec
        def aux(n : Int, next :Int, acc :Int) :Int =
        if(n == 0) acc
            else aux(n-1, acc + next,next)
        
        aux(n,1,0)