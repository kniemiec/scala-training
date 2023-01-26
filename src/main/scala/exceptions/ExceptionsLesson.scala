package exceptions

object ExceptionsLesson extends App {

  class OverflowException extends Exception
  class UnderflowException extends Exception

  class DivisionByZeroException extends Exception

  object PocketCalculator {
    def add(x: Int, y: Int): Int = {
      val result = x + y;
      if(x > 0 && y > 0 && result < 0) throw new OverflowException
      if(x < 0 && y < 0 && result > 0) throw new UnderflowException
      result
    }

    def sub(x: Int, y: Int): Int = {
      val result = x - y
      if(y > 0 && x < result) throw new UnderflowException
      if(y < 0 && x > result) throw new OverflowException
      result
    }

    def mult(x: Int, y: Int) = {
      val result = x*y
      if(x> 0 && y> 0 && result < 0) throw new OverflowException
      if(x < 0 && y < 0 && result < 0) throw new OverflowException
      if(x < 0 && y > 0 && result > 0) throw new UnderflowException
      if(x > 0 && y < 0 && result > 0) throw new UnderflowException
      result
    }

    def div(x: Int, y: Int): Int = {
      if(y == 0) throw new DivisionByZeroException
      x / y
    }
  }

  print(PocketCalculator.div(10, 0))

}
