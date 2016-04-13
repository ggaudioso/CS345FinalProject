import scala.language.implicitConversions
import scala.math.{ pow, min }

class MathCode {

  //***************************************************************************
  //* ALL Value SUBTYPES MUST IMPLEMENT THESE OPERATIONS:
  //***************************************************************************
  
  sealed trait Value {
    def + (rhs: Value):Value
    def - (rhs: Value):Value
    def * (rhs: Value):Value
    def / (rhs: Value):Value
    def ^ (rhs: Value):Value
    def OVER (rhs: Value):Value
  }
  
 
  //***************************************************************************
  //* TYPES IN OUR LANGUAGE AND THEIR OPERATORS:
  //***************************************************************************
  
  // "numbers" - Rational, bigint, bigdecimal, whatever. But an actual, known number.
  // Namely, a known number that isn't irrational
  case class NumberValue(num:BigInt, den:BigInt) extends Value {
    def + (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2 + num2*den,den*den2))
      case Unbound(sym) => simplify(Compound("+", this, sym))
      case c:Compound => simplify(Compound("+", this, c))
    }
    def - (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(this + NumberValue(-num2, den2))
      case otherwise => simplify(Compound("-", this, rhs))
    }
    def * (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num2*num, den2*den))
      case otherwise => simplify(Compound("*", this, rhs))
    }
    def / (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2, num2*den))
      case otherwise => simplify(Compound("/", this, rhs))
    }
    def ^ (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => {
        if (den2==1) 
          NumberValue(num.pow(num2.toInt), den.pow(den2.toInt))
        else
          simplify(Compound("^",NumberValue(num.pow(num2.toInt), den.pow(num2.toInt)), NumberValue(1,den2)))
      }
      case otherwise => simplify(Compound("^", this, rhs))
    }
    def OVER (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => NumberValue(num*den2, den*num2)
      case otherwise => simplify(Compound("/", this, rhs))
    }
  }
  
  //unbound variables
  case class Unbound(sym:Symbol) extends Value {
    def + (rhs: Value): Value = simplify(Compound("+",this, rhs))
    def - (rhs: Value): Value = simplify(Compound("-",this, rhs))
    def * (rhs: Value): Value = simplify(Compound("*",this, rhs))
    def / (rhs: Value): Value = simplify(Compound("/",this, rhs))
    def ^ (rhs: Value): Value = simplify(Compound("^", this, rhs))
    def OVER (rhs: Value): Value = simplify(Compound("/", this, rhs))
  }

   
  //expressions with unbound variables 
  case class Compound(op: String, lhs: Value, rhs: Value) extends Value {
    def + (rhs: Value): Value = simplify(Compound("+", this, rhs))
    def - (rhs: Value): Value = simplify(Compound("-", this, rhs))
    def * (rhs: Value): Value = simplify(Compound("*", this, rhs))
    def / (rhs: Value): Value = simplify(Compound("/", this, rhs))
    def ^ (rhs: Value): Value = simplify(Compound("^", this, rhs))
    def OVER (rhs: Value): Value = simplify(Compound("/", this, rhs))
  }
  
  
  //unary minus
  def neg(rhs:Value):Value = Compound("-",0,rhs)

  //***************************************************************************
  //* IMPLICITS:
  //***************************************************************************
  implicit def Int2Value(x:Int):NumberValue = NumberValue(x,1)
  implicit def Double2Value(x:Double):Value = {
    var decimals = (x.toString).length - (x.toString).indexOf('.') -1
    decimals = min(decimals, 4) //higher precision might cause overflow in operations :(
    val digits = pow(10,decimals)
    val num = (x*digits).toInt
    val den = digits.toInt
    simplify(NumberValue(num,den))
  }
  
  implicit def symbolToVariable(variableName:Symbol):Variable = Variable(variableName)
  implicit def symbolToAppliable(symbolicName:Symbol):Appliable = Appliable(symbolicName)
  
  // The reason we implicitly cast symbols to Unbounds instead of Values, is so that we can defer symbol lookup until
  // we actually need it (currently only when printing, but soon for function body execution).
  implicit def symbolToUnbound(symbol:Symbol):Unbound = Unbound(symbol)
  
  //***************************************************************************
  //* INSTRUCTIONS IN OUR LANGUAGE:
  //***************************************************************************
  
  case class Variable(variableName:Symbol) {
    def :=(value:Value) = {
      variableMap += (variableName -> value)
    }
  }
  
  // Because of the way our implicits work, we can't distinguish between a function call and
  // implied multiplication. E.g. 'y = 'b('x).
  // Therefore, we just try to see if it's a function, if not, it must be a multiplication (which we currently don't allow).
  case class Appliable(val applier:Symbol) {
    case class FunctionRegistration(parameterName:Symbol) {
      def :=(expression:Value) : Unit = {
        ensureValueOnlyContainsUnboundWithSymbolicName(expression, parameterName)
        functionMap += (applier -> new FunctionImplementation(parameterName, expression))
      }
    }
    
    def apply(argument:Value): Value = {
      functionMap.get(applier) match {
        case Some(implementation) => implementation.getValueFromArgument(argument)
        case None => {
          /* We could try and implement implied multiplication here */
          throw new Exception("We do not allow implied multiplication!")
        }
      }
    }
    
    def apply(parameterName:Symbol): FunctionRegistration = FunctionRegistration(parameterName)
  }
  
  
  case class FunctionImplementation(val parameterName:Symbol, val expression:Value) {
    def getValueFromArgument(value:Value) : Value = {
      // First we need to evaluate the argument
      var argument : Value = value match {
        case nv:NumberValue => nv
        case Unbound(symbol) => variableLookupFromBinding(symbol, variableMap)
        case compound:Compound => simplify(getCompoundGivenBinding(compound, false, variableMap))
      }
      
      // Here's where we can handle multiple arguments
      var bindings:Map[Symbol, Value] = Map()
      bindings += (parameterName -> argument)
      
      return expression match {
        case nv:NumberValue => nv
        case umbound:Unbound => argument // as of now we only allow one argument, and our function registration class should handle an unknown unbound
        case compound:Compound => getCompoundGivenBinding(compound, false, bindings)
      }
    }
  }
  
  //***************************************************************************
  //* PRINTING:
  //***************************************************************************
  
  val operators : String = "+-*/^" //add more if needed later
  val precedence = Array(4,4,3,3,2) //let's all stick to https://en.wikipedia.org/wiki/Order_of_operations
    // Wikipedia: The source of mathematical truth. :-)
  
  //PRINTLN syntax: PRINTLN(whatever)
  def PRINTLN(value: Value, approximate:Boolean = false): Unit =  {
    PRINT(value, approximate)
    println()
  }
  
  //PRINT syntax: PRINT(whatever)
  def PRINT(value: Value, approximate:Boolean = false): Unit = value match {
    case NumberValue(n,d) => {
      if (!approximate) { if (d==1) print(n) else print(n+"/"+d) }
      else print(n.toDouble/d.toDouble) 
    }
    case Unbound(sym) => print((sym.toString).substring(1)) //get rid of '
    case Compound(op,lhs,rhs) => {
      if (op.equals("-") && isNumberValue(lhs) && getNum(lhs) == 0) {
        print(op)
        if (isCompound(rhs)) {
          print("(")
          PRINT(rhs,approximate)
          print(")")
        }
        else 
          PRINT(rhs,approximate)
        return
      }
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
      PRINT(lhs,approximate)
      if (parlhs) print(")")
      print(" " + op + " ")
      if (parrhs) print("(")
      PRINT(rhs,approximate)
      if (parrhs) print(")")
    }
  }
  
  // PRINTLN_EVALUATES syntax: PRINTLN(whatever).. and evaluates the whatever exactly
  def PRINTLN_EVALUATE(value: Value): Unit = value match {
    case NumberValue(n,d) => println(n+"/"+d)
    case Unbound(sym) => println(sym) 
    case compund:Compound => {
      PRINT(getCompoundGivenBinding(compund, false, variableMap))
      println
    }
  }
  
    // PRINTLN_APPROXIMATE syntax: PRINTLN(whatever).. and evaluates the whatever exactly
  def PRINTLN_APPROXIMATE(value: Value): Unit = value match {
    case NumberValue(n,d) => println(n.toDouble/d.toDouble)
    case Unbound(sym) => if (isknown(sym)) println(approx(sym)) else println(sym) 
    case compound:Compound => {
      PRINTLN(getCompoundGivenBinding(compound, true, variableMap),true)
    }
  }
  
 //*****************************************************************
 //* STUFF TO DEAL WITH VARIABLES:
 //*****************************************************************  
  
  //bindings of variables stored here:
  var variableMap:Map[Symbol,Value] = Map()
  var functionMap:Map[Symbol, FunctionImplementation] = Map()
  
  //known values such as pi, e .. can add more
  //if we increase precision, increase precision of these as well.. but not too much or operations with lots of these wil overflow and mess up
  var knownVariables:Map[Symbol,Double] = Map(('e,2.7182), ('pi,3.1415))
  
  //checks if symbol is known
  def isknown(sym:Symbol): Boolean = {
    knownVariables.contains(sym)
  }
    
  //returns approximation of known symbols
  def approx(sym:Symbol): Double = knownVariables.get(sym) match {
    case Some(value) => value
    case None => 0.0 //your risk if you call on unknown symbol
  }
  
  //look up a variable given the binding
  def variableLookupFromBinding(sym:Symbol, binding:Map[Symbol, Value]):Value = {
    binding.get(sym) match {
      case Some(value) => value
      case None => Unbound(sym)
    }
  }
  
  //***************************************************************************
  //* HELPER METHODS.
  //***************************************************************************
  
  def simplify(value:Value, approximate:Boolean = false):Value = value match {
    case NumberValue(n,d) => {
      val g = gcd(n,d)
      NumberValue(n / g, d / g)
    }
    case compound:Compound => compound.op match {
      case "+" => {
        // See whether lhs can be distributed across rhs (or vice-versa)
        compound.lhs match {
          case lhs_nv:NumberValue => compound.rhs match {
            // TODO: What if both lhs2 and rhs2 are NumberValues?
            case Compound("+",lhs2:NumberValue,rhs2) => Compound("+", lhs2 + lhs_nv, rhs2)
            case Compound("+",lhs2,rhs2:NumberValue) => Compound("+", lhs2, rhs2 + lhs_nv)
            case rhs_nv:NumberValue => lhs_nv + rhs_nv
            case otherwise => compound
          }
          case otherwise => compound
        }
      }
      case "*" => {
        // See whether lhs and rhs are both NVs
        // Also, if one is an addition/subtraction then we can/should distribute
        compound.lhs match {
          case lhs_nv:NumberValue => compound.rhs match {
            case rhs_nv:NumberValue => lhs_nv * rhs_nv
            case otherwise => compound
          }
          case otherwise => compound
        }
      }
      case "/" => {
        compound.lhs match {
          case lhs_nv:NumberValue => compound.rhs match {
            case rhs_nv:NumberValue => lhs_nv / rhs_nv
            case otherwise => compound
          }
          case otherwise => compound
        }
      }
      case "^" => {
        if (approximate) {
          compound.lhs match {
          case lhs_nv:NumberValue => compound.rhs match {
            case rhs_nv:NumberValue => pow(lhs_nv.num.toDouble/lhs_nv.den.toDouble, rhs_nv.num.toDouble/rhs_nv.den.toDouble)
            case otherwise => compound
          }
          case otherwise => compound
        }
        }
        else compound
      }
      case otherwise => compound
    }
    case otherwise => value
  }
  
  //*****************************************************
  //* Runs the test() method when TEST is used in the DSL
  //*****************************************************
  def TEST() : Unit = test()
  
  /**
   * Arbitrary test method.
   */
  def test() {
    //variableLookup
    var compound1 = Compound("+", Unbound('x), NumberValue(3,1))
    var compound2 = Compound("*", NumberValue(2,1), NumberValue(2,1))
    var compound3 = Compound("*", compound1, compound2)

    
    var newCompound = simplifyCompoundNumberValuePairs(compound3);
    PRINTLN(compound3);
    PRINTLN(newCompound);
  }
  
   
  /**
   * Given a Compound, simplify all sub-Compounds which are two NumberValues.
   */
  def simplifyCompoundNumberValuePairs(compound : Compound): Value = {
    
    //println("GIVEN COMPOUND: " + compound);
    
    // Base case: both lhs and rhs are number values.
    if (isNumberValue(compound.lhs) && isNumberValue(compound.rhs)) {
      
      println("DOING: " + compound.op + ", " + compound.lhs + ", " + compound.rhs);
      
      return compound.op match {
        case "+" => compound.lhs.+(compound.rhs);
        case "-" => compound.lhs.-(compound.rhs);
        case "*" => compound.lhs.*(compound.rhs);
        case "/" => compound.lhs./(compound.rhs);
      }
    }
    
    // We will try to simplify the lhs and the rhs as much as we can.
    var newLhs = compound.lhs;
    var newRhs = compound.rhs;
    
    println("OP:  " + compound.op);
    println("LHS: " + compound.lhs);
    println("RHS: " + compound.rhs);
    
    // If the lhs is a compound, try to simplify it.
    if (isCompound(compound.lhs)) {
      newLhs = simplifyCompoundNumberValuePairs(compound.lhs.asInstanceOf[Compound]);
    }
    
    // If the rhs is a compound, try to simplify it.
    if (isCompound(compound.rhs)) {
      newRhs = simplifyCompoundNumberValuePairs(compound.rhs.asInstanceOf[Compound]);
    }
    
    //println("NEW LHS: " + newLhs);
    //println("NEW RHS: " + newRhs);
    
    // Once simplified, if both are number values, then we can return a number value.
    if (isNumberValue(newLhs) && isNumberValue(newRhs)) {
      
      println("DOING: " + compound.op + ", " + newLhs + ", " + newRhs);
      
      return compound.op match {
        case "+" => newLhs.+(newRhs);
        case "-" => newLhs.-(newRhs);
        case "*" => newLhs.*(newRhs);
        case "/" => newLhs./(newRhs);
      }
    }
    
    // They are not both number values, so return a compound.
    return Compound(compound.op, newLhs, newRhs);
  }
  

  /**
   * Given a Compound and a Binding, return a new Compound in which all unbound
   * variables are replaced by their bindings, if such a binding
   * exists.
   */
  def getCompoundGivenBinding(compound: Compound, approximate: Boolean = false, binding:Map[Symbol, Value]): Value = {
    
    // The final new lhs and rhs for this compound. These are
    // built recursively.
    var newLhs: Value = null
    var newRhs: Value = null
    var op: String = compound.op
    
    newLhs = compound.lhs match {
      case compound:Compound => getCompoundGivenBinding(compound, approximate, binding)
      case numberValue:NumberValue => numberValue
      case Unbound(unboundSymbol) => {
        if (approximate && isknown(unboundSymbol))
          approx(unboundSymbol)
        else
          variableLookupFromBinding(unboundSymbol, binding)
      }
    }
    
    newRhs = compound.rhs match {
      case compound:Compound => getCompoundGivenBinding(compound, approximate, binding)
      case numberValue:NumberValue => numberValue
      case Unbound(unboundSymbol) => {
        if (approximate && isknown(unboundSymbol))
          approx(unboundSymbol)
        else
          variableLookupFromBinding(unboundSymbol, binding)
      }
    }
    
    return simplify(Compound(op, newLhs, newRhs), approximate);
  }

  // Returns the LCM of a and b
  //def lcm(a:Int, b:Int):Int = a*b / gcd(a,b);

  //Returns the greatest common denominator of a and b
  def gcd(a:BigInt, b:BigInt):BigInt = {
    if (a < 0) { return gcd(-a,b) }
    if (b < 0) { return gcd(a,-b) }
    if (a<b) { return gcd(b,a) } else {
      var t1 = a
      var t2 = b
      while (t1 != t2) {
        if (t1 > t2) {
          t1 = t1 - t2
        } else {
          t2 = t2 - t1
        }
      }
      return t1
    }
  }
  
  // For use in function bodies, to make sure there isn't anything we don't expect
  def ensureValueOnlyContainsUnboundWithSymbolicName(value:Value, symbolicName:Symbol): Unit = {
    value match {
      case NumberValue(_,_) => return
      case Unbound(sym:Symbol) => {
        if(sym != symbolicName)
          throw new Exception("You can't have any variables in a function body other than the parameter. Parameter is " + symbolicName.toString() + ", found " + sym.toString())
      }
      case Compound(_, lhs, rhs) => {
        ensureValueOnlyContainsUnboundWithSymbolicName(lhs, symbolicName)
        ensureValueOnlyContainsUnboundWithSymbolicName(rhs, symbolicName)
      }
    }
  }
  
  //Returns true iff the given compound is made purely of NumberValues.
  def allNumberValues(compound: Value): Boolean = compound match {
    case NumberValue(_,_) => true
    case Unbound(_) => false
    case Compound(op,v1:Value,v2:Value) => allNumberValues(v1) && allNumberValues(v2)
  }

  
  //Returns true iff the given Value is of type NumberValue
  def isNumberValue(value: Value): Boolean = value match {
    case NumberValue(n,d) => true
    case otherwise => false
  }
  
  //gets numerator out of fraction
  def getNum(value: Value): BigInt = value match {
    case NumberValue(n,d) => n
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  //gets numerator out of fraction
  def getDen(value: Value): BigInt = value match {
    case NumberValue(n,d) => d
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  
  //Returns true iff the given Value is of type Compound.
  def isCompound(value: Value): Boolean = value match {
    case c:Compound => true
    case otherwise => false
  }
  
  
  //Returns true iff the given Value is of type Unbound.
  def isUnbound(value: Value): Boolean = value match {
    case u:Unbound => true
    case otherwise => false
  }
  
}
