package util

object Timer {

    def time[R](f: => R): Long = {
        val t0 = System.nanoTime()
        val r = f
        val t1 = System.nanoTime()
        val time = (t1-t0) /1000
        time
    }

    def average[R](times:Int = 10)(f: => R) : Unit = {
    
        var result = 0L
        var n = times

        while(n > 0){
            result = result + time(f) // time the function f
            n = n-1
        }
        val average = result/(times*1000d)
        println(s"time taken: $average ms ")
  }
}