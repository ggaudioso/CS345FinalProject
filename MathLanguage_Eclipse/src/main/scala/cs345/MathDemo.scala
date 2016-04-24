object MathDemo {
  
  import MathCode._ 
  
  def demo = {
    
    println("***********************************");
    println("* Examples of print and Compounds")
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
    println("* Examples of predefined variables")
    println("***********************************");
    pprint('e)
    aprint('e)
    pprint('pi)
    aprint('pi)
    println
    
    println("***********************************");
    println("* Examples of simplification")
    println("***********************************");
    //simplification of fractions and conversion of doubles
    pprint(100 OVER 5)
    pprint(3.14)
    //TODO: add examples of compound simplification 
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
    
    println("****************************************");
    println("* Examples of limits")
    println("****************************************");
    aprint(limit('n/('n-6),'n,6))
    aprint(limit(('n+3)/'n,'n,'Infinity))
    
    
  }

  def main(args: Array[String]):Unit = {  
    
    //    IF YOU DO NOT PUT A PRETTY EXAMPLE OF YOUR STUFF IN THE DEMO ABOVE,
    //       NO ONE WILL KNOW YOU DID THAT AND IT WILL NOT GO IN THE DOCUMENTATION.  
    //    So, please take a moment to look at the demo add examples of what you implemented 
    //       in the demo, and report it in the documentation. 
    //    Thanks :)   
    
    
    //demo    //comment if you don't want to see, but leave it uncommented in committed code
    //pretty examples go in the demo
    //your own testing goes here BUT is DELETED before commit :)

    pprint('x+'a+2*'x)
    
    return //move examples in demo, see google doc :D
    
  }
}
