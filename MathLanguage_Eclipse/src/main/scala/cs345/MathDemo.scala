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
    println("* Examples of numeric simplification")
    println("***********************************");
    //simplification of fractions and conversion of doubles
    pprint(100 OVER 5)
    pprint(3.14)
    println
    
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
    aprint(integrate('exp^'x,'x,-'Infinity,0))
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
    aprint(limit('exp^(-'x),'x,'Infinity))
    println
    
    println("****************************************");
    println("* Examples of summations, products and factorials")
    println("****************************************");
    pprint(summation('n,'n,0,'Infinity))
    aprint(summation(1/(2^'n),'n,0,'Infinity))
    aprint(product('i,'i,1,5))
    aprint(factorial(7))
    println
    
    println("****************************************");
    println("* Examples of predefined functions")
    println("****************************************");
    pprint(derive('cos^'x,'x))
    aprint(integrate('exp^'x,'x,3,5))   
    println
    
    
    println("****************************************");
    println("* Examples of simplification")
    println("****************************************");
    'cmp1 := 'mike + 3;
    'cmp2 := 4 - 'mike;
    'cmp3 := 'cmp1 - 'cmp2;
    // ((mike + 3) - (4 - mike)) => mike + -1 + mike
    pprint('cmp3);
    
    'cmp4 := 55 + 'cmp1;
    'cmp5 := 'cmp4 - 'cmp2;
    // ((55 + (mike + 3)) - (4 - mike)) => 54 + mike + mike
    pprint('cmp5);
    
    'cmp6 := 'mike - 'mike;
    'cmp7 := 'cmp6 + 'cmp6;
    // ((mike - mike) + (mike - mike)) => 0
    pprint('cmp6);
    
    'cmp8 := 'cmp3 * 'cmp5;
    // (((mike + 3) - (4 - mike)) * ((55 + (mike + 3)) - (4 - mike))) =>
    // mike * mike + mike * -1 + 54 * mike + mike * mike + mike * mike + mike * -1 + 54 * mike + mike * mike + -54
    pprint('cmp8);
    
    'cmp9 := 'cmp1 + 'cmp2;
    'cmp10 := 'cmp9 * 'cmp5;
    // (((mike + 3) + (4 - mike)) * ((55 + (mike + 3)) - (4 - mike))) => mike * 7 + 378 + mike * 7
    pprint('cmp10);
    
    'cmp11 := 'mike + 1;
    'cmp12 := 'cmp11 - 'mike;
    'cmp13 := 'cmp12 ^ 3
    // (((mike + 1) - mike) ^ 3) => 1
    pprint('cmp13);
    
    'cmp14 := 'cmp12 ^ 'james;
    // (((mike + 1) - mike) ^ james) => 1
    pprint('cmp14);
    
    'cmp15 := 'mike + 'james;
    'cmp16 := 'cmp15 + 'taylorSwift;
    'cmp17 := 'cmp16 + 'selenaGomez;
    // (((mike + james) + taylorSwift) + selenaGomez) => mike + james + taylorSwift + selenaGomez
    pprint('cmp17);
    
    //b * 2 + a * 2 + 8 - 1
    'cmp18 := 'b * 2;
    'cmp19 := 'a * 2;
    'cmp20 := 'cmp18 + 'cmp19;
    'cmp21 := 'cmp20 + 8;
    'cmp22 := 'cmp21 - 1;
    // ((((b * 2) + (a * 2) + 8) - 1) => b * 2 + a * 2 + 7
    pprint('cmp22);
    
    'cmp23 := 1 + 'b + 'c + 7;
    // (((1 + b) + c) + 7) => 8 + b + c
    pprint('cmp23);
    
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
