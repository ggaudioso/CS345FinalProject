
object MathDemo extends MathCode{
  def main(args: Array[String]):Unit = {  
    
    'b := 3 + 6 * 5 / 3
    
    'x := -'b + 6 * 7 / 6 * 3
    
    'f of 'x := -('x + 2)
    
    'g of ('x, 'y) := 'x + 'y
    
    
    println
    PRINTLN_EVALUATE(1 / 3)
    PRINTLN_EVALUATE(1 OVER 3)

    println
    PRINTLN_EVALUATE((1 OVER 3) - (3 OVER 1))

    println
    'c := 4
    'a := 'c + 3
    PRINTLN_EVALUATE('a)

    println
    PRINTLN_EVALUATE(('d+'y)*('z+'w))
    PRINTLN_EVALUATE(('d-'y)*('z+'w))
    PRINTLN_EVALUATE(('d-'y)*('z-'w))
    PRINTLN_EVALUATE('z*('d-'y))

    println
    PRINTLN(1^'a)
    
//
//    LET('three) BE 3
//    LET('four) BE 4
//    
//    PRINTLN_EVALUATE('three/'four)
//    PRINTLN_APPROXIMATE('three/'four)
//    
//    PRINTLN('three)
//    
//    PRINTLN(neg('x + 4))
//    
//    PRINTLN('e ^ neg('x))
//    
//    //PRINT('four)
//    //PRINT('newvar)
//    LET('x) BE 'newvar + 'four
//    PRINTLN('x)
//    LET('x) BE 'x + 2
//    PRINTLN('x) //eg of what needs to be simplified
//    PRINTLN('x * 2)
//    
//
//    
//    
//    PRINTSTRING("\nNew update, see below:\n")
//    
//    
//    // Mike:
//    // This will print: 'b + 3
//    LET ('a) BE 'b + 3
//    PRINTSTRING("Expression with 'b unbound:")
//    PRINTLN('a)
//    
//    // This will still print: 'b + 3
//    // It should print: 7
//    //   since 'b was assigned to 4.
//    PRINTSTRING("\nBinding 'b to 7.\n")
//    LET ('b) BE 4
//    PRINTSTRING("Old way:")
//    PRINTLN('a)
//    
//    // This method performs correctly. I made it separately
//    // in case we still want the old behavior for some reason.
//    PRINTSTRING("New way (success):")
//    PRINTLN_USE_BINDINGS('a)
//    
//    PRINTSTRING("\n")
//    
//    // Testing further.
//    PRINTSTRING("More advanced test below:")
//    LET ('c) BE 'a + 'd
//    LET ('e) BE 'c * 'f
//    LET ('g) BE 'e / 36
//    PRINTLN_USE_BINDINGS('g)
//    LET ('d) BE 45
//    PRINTLN_USE_BINDINGS('g)
//    LET ('f) BE 56
//    PRINTLN_USE_BINDINGS('g)
    
    //'x := 'a + 'b + 'c + 'd
    //PRINTLN('x)
    
    'y := 'f('x)
    'h := 'f(2 + 80)
    'z := 'f('x + 5)
    'p := 'g('f('x), 2)
    
    
    PRINTLN('b)
    PRINTLN('y)
    PRINTLN('h)
    PRINTLN('z)
    PRINTLN('p)

    println
    PRINTLN(DERIVE('w + (5*'w^3), 'w))
    PRINTLN(DERIVE('w + 5*'w^3, 'w))
    PRINTLN('w + 5*'w^3)
    PRINTLN('x)
    println
    PRINTLN('x + 5*'x^3)
    'd := 'x + 5*'x^3
    PRINTLN('d)
    
    // Regression tests for compound simplification.
    //TEST_COMPOUND_SIMPLIFICATION
  }
}
