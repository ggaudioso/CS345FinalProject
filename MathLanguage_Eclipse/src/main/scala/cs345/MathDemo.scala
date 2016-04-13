
object MathDemo extends MathCode{
  def main(args: Array[String]):Unit = {  
    
    
    PRINTLN_APPROXIMATE('pi)
    PRINTLN_APPROXIMATE('pi+3)
    PRINTLN_EVALUATE('pi+3)
    
    'x := 'pi * 2
    PRINTLN_APPROXIMATE('x*'r)
    println
    PRINTLN_EVALUATE('e+'pi)
    PRINTLN_APPROXIMATE('e+'pi)
    println
    PRINTLN_EVALUATE('e^'pi)
    PRINTLN_APPROXIMATE('e^'pi)
    
    println
    PRINTLN_EVALUATE(1 / 3)
    PRINTLN_EVALUATE(1 OVER 3)

    println
    PRINTLN_EVALUATE((1 OVER 3) - (3 OVER 1))
  }
}
