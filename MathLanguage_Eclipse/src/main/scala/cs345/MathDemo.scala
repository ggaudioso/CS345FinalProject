object MathDemo {
  
  import MathCode._ 

  def main(args: Array[String]):Unit = {  
    
    //print examples:
     'print := 'a + 4 + 'b
    //1. pretty print
    pprint('print*2 -1)
    //2. approximator print
    aprint(1 OVER 3 + 'e) 
    //3. verbose print (old PRINTLN)
    vprint('print*2 -1)
    
    //simplification of fractions, and conversion of doubles
    pprint(100 OVER 5)
    pprint(3.14)
    
    return //move examples before the return, see google doc :D
    
    'b := 3 + 6 * 5 / 3
    
    'x := -'b + 6 * 7 / 6 * 3
    
    'f of 'x := -('x + 2)
    
    'g of ('x, 'y) := 'x + 'y
    
    
   
    println
    pprint(1 / 3)
    pprint(1 OVER 3)

    println
    pprint((1 OVER 3) - (3 OVER 1))

    println
    'c := 4
    'a := 'c + 3
    pprint('a)

    println
    pprint(('d+'y)*('z+'w))
    pprint(('d-'y)*('z+'w))
    pprint(('d-'y)*('z-'w))
    pprint('z*('d-'y))

    println
    vprint(1^'a)
    
    'y := 'f('x)
    'h := 'f(2 + 80)
    'z := 'f('x + 5)
    'p := 'g('f('x), 2)
    
    
    vprint('b)
    vprint('y)
    vprint('h)
    vprint('z)
    vprint('p)

    println
    vprint(DERIVE('w + (5*'w^3), 'w))
    vprint(DERIVE('w + 5*'w^3, 'w))
    vprint('w + 5*'w^3)
    vprint('x)
    println
    vprint('x + 5*'x^3)
    'd := 'x + 5*'x^3
    vprint('d)
    

  }
}
