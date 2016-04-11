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
  
  //PRINTLN syntax: PRINTLN(whatever)
  def PRINTLN(value: Value): Unit = value match {
    case IntValue(intNum) => println(intNum)
    case DoubleValue(realNum) => println(realNum)
    case Unbound(sym) => println(sym) 
    case Compound(op,lhs,rhs) => {
      PRINT(simplify(value))
      println
    }
  }
  
  // PRINTLN_USE_BINDINGS syntax: PRINTLN(whatever)
  def PRINTLN_USE_BINDINGS(value: Value): Unit = value match {
    case IntValue(intNum) => println(intNum)
    case DoubleValue(realNum) => println(realNum)
    case Unbound(sym) => println(sym) 
    case Compound(op,lhs,rhs) => {
      PRINT(getCompoundWithBindings(value.asInstanceOf[Compound]))
      println
    }
  }
  
  
  //PRINT syntax: PRINT(whatever)
  def PRINT(value: Value): Unit = value match {
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
   
  
  // PRINTSTRING syntax: PRINTSTRING(myString: String)
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
    //variableLookup
    
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
   * Given a Compound, return a new Compound in which all unbound
   * variables are replaced by their bindings, if such a binding
   * exists.
   */
  def getCompoundWithBindings(compound: Compound): Compound =
  {
    // The final new lhs and rhs for this compound. These are
    // built recursively.
    var newLhs: Value = null;
    var newRhs: Value = null;
    var op: String = compound.op;
    
    // If the lhs is a compound, recursively call it.
    if (isCompound(compound.lhs))
    {
      newLhs = getCompoundWithBindings(compound.lhs.asInstanceOf[Compound]);
    }
    // Else if lhs is a double of int, don't change it.
    else if (isDoubleValue(compound.lhs) || isIntValue(compound.lhs))
    {
      newLhs = compound.lhs;
    }
    // Else it is a variable, and we can try to replace it with a binding,
    // if one exists..
    else
    {
      newLhs = variableLookup(compound.lhs.asInstanceOf[Unbound].sym);
    }
    
    // If the rhs is a compound, recursively call it.
    if (isCompound(compound.rhs))
    {
      newRhs = getCompoundWithBindings(compound.rhs.asInstanceOf[Compound]);
    }
    // Else if rhs is a double of int, don't change it.
    else if (isDoubleValue(compound.rhs) || isIntValue(compound.rhs))
    {
      newRhs = compound.rhs;
    }
    // Else it is a variable, and we can try to replace it with a binding,
    // if one exists..
    else
    {
      newRhs = variableLookup(compound.rhs.asInstanceOf[Unbound].sym);
    }
    
    //print("NEW LHS: ");
    //println(newLhs);
    //print("NEW RHS: ");
    //println(newRhs);
    
    return Compound(op, newLhs, newRhs);
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
    
    // One of the sides is not a double or a real.
    else
    {
      // Assume both lhs and rhs are not all terminals.
      var lhsAllTerminals: Boolean = false;
      var rhsAllTerminals: Boolean = false;
      
      // If we already know lhs or rhs is all terminals.
      if (lhsTerminal)
      {
        lhsAllTerminals = true;
      }
      
      if (rhsTerminal)
      {
        rhsAllTerminals = true;
      }
      
      // If lhs is a compound, then it is not a terminal. Thus,
      // recursively check if it is made of all terminals.
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
      
      // If rhs is a compound, then it is not a terminal. Thus,
      // recursively check if it is made of all terminals.
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
      
      // If they're not both all terminals, return false.
      if (!lhsAllTerminals || !rhsAllTerminals)
      {
        return false;
      }
      else
      {
        // Both lhs and rhs are all terminals.
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
  
  
  /**
   * Returns true iff the given Value is of type Unbound.
   */
  def isUnbound(value: Value): Boolean =
  {
    if (value.isInstanceOf[Unbound])
    {
      return true;
    }
    else
    {
      return false;
    }
  }
}

