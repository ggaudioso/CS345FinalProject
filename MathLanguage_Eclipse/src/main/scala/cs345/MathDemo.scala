
object MathDemo extends MathCode{
  def main(args: Array[String]):Unit = {  
    'b := 3 + 6 * 5 / 3
    
    'x := -'b + 6 * 7 / 6 * 3
    
    'f of 'x := -('x + 2)
    
    'g of ('x, 'y) := 'x + 'y
    
    'y := 'f('x)
    'h := 'f(2 + 80)
    'z := 'f('x + 5)
    'p := 'g('f('x), 2)
    
    
    PRINTLN('b)
    PRINTLN('y)
    PRINTLN('h)
    PRINTLN('z)
    PRINTLN('p)
  }
}
