
object MathDemo extends MathCode{
  def main(args: Array[String]):Unit = {

    LET('three) BE 3.0
    LET('four) BE 2 + 2
    //PRINT('four)
    //PRINT('newvar)
    LET('x) BE 'newvar + 'four
    PRINTLN('x)
    LET('x) BE 'x + 2
    PRINTLN('x) //eg of what needs to be simplified
    PRINTLN('x * 2)
    

    
    PRINTSTRING("\nNew update, see below:\n")
    
    
    
    // Mike:
    // This will print: 'b + 3
    LET ('a) BE 'b + 3
    PRINTSTRING("Expression with 'b unbound:")
    PRINTLN('a)
    
    // This will still print: 'b + 3
    // It should print: 7
    //   since 'b was assigned to 4.
    PRINTSTRING("\nBinding 'b to 7.\n")
    LET ('b) BE 4
    PRINTSTRING("Old way:")
    PRINTLN('a)
    
    // This method performs correctly. I made it separately
    // in case we still want the old behavior for some reason.
    PRINTSTRING("New way (success):")
    PRINTLN_USE_BINDINGS('a)
    
    PRINTSTRING("\n")
    
    // Testing further.
    PRINTSTRING("More advanced test below:")
    LET ('c) BE 'a + 'd
    LET ('e) BE 'c * 'f
    LET ('g) BE 'e / 36
    PRINTLN_USE_BINDINGS('g)
    LET ('d) BE 45
    PRINTLN_USE_BINDINGS('g)
    LET ('f) BE 56
    PRINTLN_USE_BINDINGS('g)
    
    
    //TEST
    
    
 
  }
}