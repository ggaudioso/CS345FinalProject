
object MathDemo extends MathCode{
  def main(args: Array[String]):Unit = {  
    'b := 3 + 6
    
    'x := 'b
    
    'f('x) := 'x + 2
    
    'y := 'f('x)
    'h := 'f(2 + 80)
    'z := 'f('x + 5)
    
    PRINTLN('f)
    
    
    PRINTLN('y)
    PRINTLN('h)
    PRINTLN('z)
  }
}
