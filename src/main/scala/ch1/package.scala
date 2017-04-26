package object ch1 {
  def fib(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case _ => fib(n - 1) + fib(n - 2)
  }
}

object App extends App {
  print(ch1.fib(10))
}
