import scala.language.implicitConversions

class MathCode {
  
  //value can be integer (eg 3), double (eg 3.0), 
  //unbound variable (eg x), or expression with variable (eg x+1) 
  sealed trait Value {
    def + (rhs: Value):Value
    def - (rhs: Value):Value
    def * (rhs: Value):Value
    def / (rhs: Value):Value
  }
  
  
  
 //TYPES IN OUR LANGUAGE AND THEIR OPERATORS:
  
  //integers
  case class IntValue(val x:Int) extends Value {
    def + (rhs: Value):Value = rhs match{
      case IntValue(y) => IntValue(x + y)
      case DoubleValue(y) => DoubleValue(x + y)
      case Unbound(sym) => Compound("+", this, sym)
      case Compound(op,lhs,rhs) => Compound("+", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
    def - (rhs: Value):Value = rhs match{
      case IntValue(y) => IntValue(x - y)
      case DoubleValue(y) => DoubleValue(x - y)
      case Unbound(sym) => Compound("-", this, sym)
      case Compound(op,lhs,rhs) => Compound("-", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
    def * (rhs: Value):Value = rhs match{
      case IntValue(y) => IntValue(x * y)
      case DoubleValue(y) => DoubleValue(x * y)
      case Unbound(sym) => Compound("*", this, sym)
      case Compound(op,lhs,rhs) => Compound("*", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
    def / (rhs: Value):Value = rhs match{
      case IntValue(y) => DoubleValue(1.0*x / y) //double because integer division is DIV, not / in math 
      case DoubleValue(y) => DoubleValue(x / y)
      case Unbound(sym) => Compound("/", this, sym)
      case Compound(op,lhs,rhs) => Compound("/", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
  }
  implicit def Int2Value(x:Int) = IntValue(x)

  
  //reals
  case class DoubleValue(x:Double) extends Value {
    def + (rhs: Value):Value = rhs match{
      case IntValue(y) => DoubleValue(x + y)
      case DoubleValue(y) => DoubleValue(x + y)
      case Unbound(sym) => Compound("+", this, sym)
      case Compound(op,lhs,rhs) => Compound("+", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
    def - (rhs: Value):Value = rhs match{
      case IntValue(y) => DoubleValue(x - y)
      case DoubleValue(y) => DoubleValue(x - y)
      case Unbound(sym) => Compound("-", this, sym)
      case Compound(op,lhs,rhs) => Compound("-", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
    def * (rhs: Value):Value = rhs match{
      case IntValue(y) => DoubleValue(x * y)
      case DoubleValue(y) => DoubleValue(x * y)
      case Unbound(sym) => Compound("*", this, sym)
      case Compound(op,lhs,rhs) => Compound("*", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
    def / (rhs: Value):Value = rhs match{
      case IntValue(y) => DoubleValue(x / y)
      case DoubleValue(y) => DoubleValue(x / y)
      case Unbound(sym) => Compound("/", this, sym)
      case Compound(op,lhs,rhs) => Compound("/", this, Compound(op,lhs,rhs)) //TODO: algebraic simplification
    }
  }
  implicit def Double2Value(x:Double) = DoubleValue(x)
 
  
  //unbound variables
  case class Unbound(sym:Symbol) extends Value {
     //TODO: might want to simplify unbounds here algebraically, by looking at what rhs is
    //eg: a  + 2-a should simplify to 2...
    def + (rhs: Value): Value = Compound("+",this, rhs)
    def - (rhs: Value): Value = Compound("-",this, rhs)
    def * (rhs: Value): Value = Compound("*",this, rhs)
    def / (rhs: Value): Value = Compound("/",this, rhs)
  }

   
  //expressions with unbound variables 
  case class Compound(op: String, lhs: Value, rhs: Value) extends Value {
    //TODO: simplify unbounds here algebraically in all cases of op
    //eg: a+2 + 2 should simplify to a+4...
    def + (rhs: Value): Value = Compound("+", this, rhs) 
    def - (rhs: Value): Value = Compound("-", this, rhs) 
    def * (rhs: Value): Value = Compound("*", this, rhs) 
    def / (rhs: Value): Value = Compound("/", this, rhs) 
  }
  
  
  
  
 //SET OF INSTRUCTIONS IN OUR LANGUAGE:
  
  //Assignment syntax: LET (symbol) BE value
  case class LET(sym:Symbol) {
    def BE (value:Value) = {
      scope += (sym -> value)
    }
  }
  
  //Println syntax: PRINTLN (whatever)
  def PRINTLN(value: Value):Unit = value match {
    case IntValue(intNum) => println(intNum)
    case DoubleValue(realNum) => println(realNum)
    case Unbound(sym) => println(sym) 
    case Compound(op,lhs,rhs) => {
      //TODO: add structure when needed with parenthesis
      //eg: compound(*, compound(+, x, 4), 2) now is x + 4 * 2 but should be (x + 4) * 2
      PRINT(lhs)
      print(" " + op + " ")
      PRINT(rhs)
      println;
    } 
  }
  
   //Print syntax: PRINT (whatever)
   def PRINT(value: Value):Unit = value match {
    case IntValue(intNum) => print(intNum)
    case DoubleValue(realNum) => print(realNum)
    case Unbound(sym) => print(sym) 
    case Compound(op,lhs,rhs) => {
      PRINT(lhs)
      print(" " + op + " ")
      PRINT(rhs)
    } 
  }
   
  
  
   
 //STUFF TO DEAL WITH VARIABLES:
   
  //bindings of variables stored here:
  var scope:Map[Symbol,Value] = Map()
  
  //look up a variable in our bindings
  implicit def variableLookup(sym:Symbol):Value = scope.get(sym) match {
    case Some(value) => value
    case None => Unbound(sym)
  }
}
