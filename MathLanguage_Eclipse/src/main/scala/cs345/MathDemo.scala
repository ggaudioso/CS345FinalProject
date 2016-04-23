object MathDemo {
  
  import MathCode._ 
  
  def demo = {
    
    println("***********************************");
    println("* Examples of print")
    println("***********************************");
    //print examples:
     'print := 'a + 4 + 'b
    //1. pretty print
    pprint('print*2 -1)
    //2. approximate print
    aprint(1 OVER 3 + 'e) 
    //3. verbose print (old PRINTLN)
    vprint('print*2 -1)
    println
    
    println("***********************************");
    println("* Examples of simplification")
    println("***********************************");
    //simplification of fractions and conversion of doubles
    pprint(100 OVER 5)
    pprint(3.14)
    println
    
    // TODO: Add the examples from the doc (which are also in Mike's
    // regression tests.  Note: Many of them do not work currently
    // due to bugs in simplify().
    
    println("***********************************");
    println("* Examples of derivation")
    println("***********************************");
    'f of 'x := -(2*'x + 2)
    print("Function:   ");
    pprint('f('x))
    print("Derivative: ");
    pprint(derive('f('x), 'x))
    println
    
    println("****************************************");
    println("* Examples of indefinite integrals")
    println("****************************************");
    pprint(integrate('x^2, 'x))
    pprint(integrate('x*2, 'x))
    println
    
    println("****************************************");
    println("* Examples of definite integrals")
    println("****************************************");
    pprint(integrate('x, 'x, 'a, 'b))
    print("Integral on [0, 5]: ");
    aprint(integrate('x + ('x^2) + (2^'x), 'x, 0, 5)) //you need to put parenthesis or Scala is weird
    println
 
    println("****************************************");
    println("* Examples of piecewise functions")
    println("****************************************");
    // Piecewise
    'p of ('x, 'y) when ('x + 1 === 0 + 'y, 'x > 0) := 0
    'p of ('x, 'y) when ('x !== 0) := 1 / 'x
    pprint('p(45, 46))
    pprint('p(4, 8))
    println

    println("****************************************");
    println("* Examples of solving a simple equation")
    println("****************************************");
    pprint(solve(5-2*'x,'a/'b,'x))
    println
  }

  def main(args: Array[String]):Unit = {  
    
    demo    //comment if you don't want to see, but leave it uncommented in committed code
    //pretty examples go in the demo
    //your own testing goes here BUT is moved after the return before commit because I might not want to see it :)
    
    
    return //move examples in demo, see google doc :D
    
        'g of 'x := -(2*'x + 2)
    pprint(derive('g('x),'x))

    println
    println

    pprint('a+1+2)
    pprint('a+1+'a)
    pprint(('a - 3)-(4 - 'a))

    println
    println

    pprint('a-'a)
    pprint('a+'a)
    pprint('a*0)
    pprint('a*1)
    pprint('a+0)
    pprint(2*'a-2*'a)
    pprint('a+'a-'a)
    pprint('a+1+1)
    println
    
    
    
    
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
    vprint(derive('w + (5*'w^3), 'w))
    vprint(derive('w + 5*'w^3, 'w))
    vprint('w + 5*'w^3)
    vprint('x)
    println
    vprint('x + 5*'x^3)
    'd := 'x + 5*'x^3
    vprint('d)
    
    'abc of ('x, 'y) when ('x + 1 === 0 + 'y, 'x > 0) := 0
    'abc of ('x, 'y) when ('x !== 0) := 1 / 'x
    
    'test := 45
    
    pprint('abc('test + 5, 46))
    pprint('abc(4, 8))
  }
}
