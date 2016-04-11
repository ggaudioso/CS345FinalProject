import scala.language.implicitConversions

class MathCode {
  
  //value can be integer (e.g., 3), double (e.g., 3.0), 
  //unbound variable (e.g., x), or expression with variable (e.g., x+1) 
  sealed trait Value {
    def + (rhs: Value):Value
    def - (rhs: Value):Value
    def * (rhs: Value):Value
    def / (rhs: Value):Value
  }
  
 
  //***************************************************************************
  //* TYPES IN OUR LANGUAGE AND THEIR OPERATORS:
  //***************************************************************************
  
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
  
  
  
  //***************************************************************************
  //* INSTRUCTIONS IN OUR LANGUAGE:
  //***************************************************************************
  
  //Assignment syntax: LET (symbol) BE value
  case class LET(sym:Symbol) {
    def BE (value:Value) = {
      scope += (sym -> value)
    }
  }
  
  
  val operators : String = "+-*/" //add more if needed later
  val precedence = Array(4,4,3,3) //let's all stick to https://en.wikipedia.org/wiki/Order_of_operations
  
  //PRINTLN syntax: PRINTLN (whatever)
  def PRINTLN(value: Value):Unit = value match {
    case IntValue(intNum) => println(intNum)
    case DoubleValue(realNum) => println(realNum)
    case Unbound(sym) => println(sym) 
    case Compound(op,lhs,rhs) => {
      PRINT(simplify(value))
      println
    }
  }
  
  //PRINT syntax: PRINT (whatever)
  def PRINT(value: Value):Unit = value match {
    case IntValue(intNum) => print(intNum)
    case DoubleValue(realNum) => print(realNum)
    case Unbound(sym) => print(sym) 
    case Compound(op,lhs,rhs) => {
      var parlhs = false 
      var parrhs = false
      lhs match {
        case Compound(lop,llhs,lrhs) => {
          if (precedence(operators.indexOf(lop)) > precedence(operators.indexOf(op))) 
            parlhs = true
        }
        case _ => parlhs = false
      }
      rhs match {
        case Compound(rop,rlhs,rrhs) => {
          if (precedence(operators.indexOf(rop)) > precedence(operators.indexOf(op)))
            parrhs = true
        }
        case _ => parrhs = false
      }
      if (parlhs) print("(")
      PRINT(lhs)
      if (parlhs) print(")")
      print(" " + op + " ")
      if (parrhs) print("(")
      PRINT(rhs)
      if (parrhs) print(")")
    }
  }
   
  
  // println method for Strings
  def PRINTSTRING(value : String) : Unit = println(value)
  
  
  
 //*****************************************************************
 //* STUFF TO DEAL WITH VARIABLES:
 //*****************************************************************  
  
  //bindings of variables stored here:
  var scope:Map[Symbol,Value] = Map()
  
  //look up a variable in our bindings
  implicit def variableLookup(sym:Symbol):Value = scope.get(sym) match {
    case Some(value) => value
    case None => Unbound(sym)
  }
  
    
  

  
  //***************************************************************************
  //* HELPER METHODS.
  //***************************************************************************
  
  
  def simplify(value:Value):Value = {
    value match {
      case IntValue(i) => new IntValue(i)
      case DoubleValue(d) => new DoubleValue(d)
      case Unbound(s) => new Unbound(s)
      case Compound(op,lhs,rhs) => {
        value //jk, need to do a lot here :)
      }
    }
  }
  
  
  
  
  //*******************************
  //* Runs the test() method when
  //* used in the DSL
  //*******************************
  def TEST() : Unit = test()
  
  /**
   * Arbitrary test method.
   */
  def test()
  {
    var compound1 = Compound("+", IntValue(1), IntValue(3))
    var compound2 = Compound("-", compound1, IntValue(45))
    var compound3 = Compound("*", compound1, compound2)
    if (allIntOrDoubles(compound3))
    {
      PRINTLN(compound3)
      PRINTSTRING("SUCCESS")
    }
    else
    {
      PRINTLN(compound3)
      PRINTSTRING("FAIL")
    }
    
  }
   
  
  /**
   * Returns true iff the given compound is made purely of
   * IntValues or DoubleValues
   */
  def allIntOrDoubles(compound: Compound): Boolean =
  {
    // Base case: Both LHS and RHS are doubles or reals.
    var lhsTerminal: Boolean = isIntValue(compound.lhs) || isDoubleValue(compound.lhs);
    var rhsTerminal: Boolean = isIntValue(compound.rhs) || isDoubleValue(compound.rhs);
    
    if (lhsTerminal && rhsTerminal)
    {
      return true;
    }
    
    // Otherwise, make sure each side is a compound and recursively
    // check if it is made of all terminals.
    else
    {
      // Assume both lhs and rhs are not all terminals.
      var lhsAllTerminals: Boolean = false;
      var rhsAllTerminals: Boolean = false;
      
      if (lhsTerminal)
      {
        lhsAllTerminals = true;
      }
      
      if (rhsTerminal)
      {
        rhsAllTerminals = true;
      }
      
      // Find out if the lhs is all terminals.
      if (isCompound(compound.lhs))
      {
        if (allIntOrDoubles(compound.lhs.asInstanceOf[Compound]))
        {
          lhsAllTerminals = true
        }
        else
        {
          lhsAllTerminals = false;
        }
      }
      
      // Find out if the rhs is all terminals.
      if (isCompound(compound.rhs))
      {
        if (allIntOrDoubles(compound.rhs.asInstanceOf[Compound]))
        {
          rhsAllTerminals = true
        }
        else
        {
          rhsAllTerminals = false;
        }
      }
      
      // If the lhs or rhs was not a compound, then 
      
      
      // If they're not both all terminals, return false.
      if (!lhsAllTerminals || !rhsAllTerminals)
      {
        return false;
      }
      else
      {
        return true;
      }
      
    }
  }
  
  
  /**
   * Returns true iff the given Value is of type IntValue.
   */
  def isIntValue(value: Value): Boolean =
  {
    if (value.isInstanceOf[IntValue])
    {
      return true;
    }
    else
    {
      return false;
    }
  }
  
  
  /**
   * Returns true iff the given Value is of type DoubleValue.
   */
  def isDoubleValue(value: Value): Boolean =
  {
    if (value.isInstanceOf[DoubleValue])
    {
      return true;
    }
    else
    {
      return false;
    }
  }
  
  /**
   * Returns true iff the given Value is of type Compound.
   */
  def isCompound(value: Value): Boolean =
  {
    if (value.isInstanceOf[Compound])
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}