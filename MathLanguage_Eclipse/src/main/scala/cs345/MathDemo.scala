
object MathDemo extends MathCode{
  def main(args: Array[String]):Unit = {

    LET('three) BE 3.0
    LET('four) BE 2 + 2
    //PRINT('four)
    //PRINT('newvar)
    LET('x) BE 'newvar + 'four
    PRINTLN('x)
    PRINTLN('x + 2) //eg of what needs to be simplified
    PRINTLN('x * 2) //eg of need of parenthesis in print sometimes
    
    PRINTSTRING("\nPrinting a string\n")
 
  }
}