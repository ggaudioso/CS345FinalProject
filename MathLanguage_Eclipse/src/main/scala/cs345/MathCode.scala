import scala.language.implicitConversions
import scala.math.{ pow, min }

object MathCode {

  //***************************************************************************
  //* ALL Value SUBTYPES MUST IMPLEMENT THESE OPERATIONS:
  //***************************************************************************
  
  trait Value {
    def + (rhs: Value):Value
    def - (rhs: Value):Value
    def * (rhs: Value):Value
    def / (rhs: Value):Value
    def ^ (rhs: Value):Value
    def unary_-(): Value = Compound("-", 0, this)
    def unary_+(): Value = this
    def OVER (rhs: Value):Value
  }
  
 
  //***************************************************************************
  //* TYPES IN OUR LANGUAGE AND THEIR OPERATORS:
  //***************************************************************************

  // "numbers" - Rational, bigint, bigdecimal, whatever. But an actual, known number.
  // Namely, a known number that isn't irrational
  case class NumberValue(val num:BigInt, val den:BigInt) extends Value {
    def + (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => NumberValue(num*den2 + num2*den,den*den2)
      case Unbound(sym) => Compound("+", this, sym)
      case c:Compound => Compound("+", this, c)
    }
    def - (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => this + NumberValue(-num2, den2)
      case otherwise => Compound("-", this, rhs)
    }
    def * (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => NumberValue(num2*num, den2*den)
      case otherwise => Compound("*", this, rhs)
    }
    def / (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => NumberValue(num*den2, num2*den)
      case otherwise => Compound("/", this, rhs)
    }
    def ^ (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => {
        if (den2==1) 
          NumberValue(num.pow(num2.toInt), den.pow(den2.toInt))
        else
          Compound("^",NumberValue(num.pow(num2.toInt), den.pow(num2.toInt)), NumberValue(1,den2))
      }
      case otherwise => Compound("^", this, rhs)
    }
    def OVER (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => NumberValue(num*den2, den*num2)
      case otherwise => Compound("/", this, rhs)
    }
    
    override def toString(): String = {
       if (den == 1)
         return num.toString() 
       else {
         return (num + "/" + den).toString()  
       }
    }
  }
  
  //unbound variables
  case class Unbound(val sym:Symbol) extends Value {
    def + (rhs: Value): Value = Compound("+",this, rhs)
    def - (rhs: Value): Value = Compound("-",this, rhs)
    def * (rhs: Value): Value = Compound("*",this, rhs)
    def / (rhs: Value): Value = Compound("/",this, rhs)
    def ^ (rhs: Value): Value = Compound("^", this, rhs)
    def OVER (rhs: Value): Value = Compound("/", this, rhs)
    
    // Gets rid of '.
    override def toString(): String = return (sym.toString).substring(1) 
  }

   
  //expressions with unbound variables 
  case class Compound(val op: String, val lhs: Value, val rhs: Value) extends Value {
    def + (rhs: Value): Value = Compound("+", this, rhs)
    def - (rhs: Value): Value = Compound("-", this, rhs)
    def * (rhs: Value): Value = Compound("*", this, rhs)
    def / (rhs: Value): Value = Compound("/", this, rhs)
    def ^ (rhs: Value): Value = Compound("^", this, rhs)
    def OVER (rhs: Value): Value = Compound("/", this, rhs)
    
    
    override def toString(): String = {
      
      // This will return either a NumberValue, an UnBound, or a 
      // CompoundCluster.
      var value: Value = Simplifier.simplifyCompoundtoCompoundCluster(this, variableMap) 
      
      return value.toString() 
      
      // If we want a fully-parenthesized left-associative version.
      //return flattenCompoundToString(this) 
    }
  }
 

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
    NumberValue(num,den)
  }
  implicit class Int2NV(v:Int) {
    def + (rhs: Symbol): Value = NumberValue(v, 1) + Unbound(rhs)
    def - (rhs: Symbol): Value = NumberValue(v, 1) - Unbound(rhs)
    def * (rhs: Symbol): Value = NumberValue(v, 1) * Unbound(rhs)
    def / (rhs: Symbol): Value = NumberValue(v, 1) / Unbound(rhs)
    def ^ (rhs: Symbol): Value = NumberValue(v, 1) ^ Unbound(rhs)
  }

  implicit def symbolToVariable(variableName:Symbol):Variable = Variable(variableName)
  implicit def symbolToAppliable(symbolicName:Symbol):Function = Function(symbolicName)
  implicit def symbolToFunctionRegistration(symbolicName:Symbol):FunctionRegistration = FunctionRegistration(symbolicName)
  
  // The reason we implicitly cast symbols to Unbounds instead of Values, is so that we can defer symbol lookup until
  // we actually need it.
  implicit def symbolToUnbound(symbol:Symbol):Unbound = Unbound(symbol)
  
  //implicit def compoundClusterToCompound(cc:CompoundCluster):Compound = Simplifier.compoundClusterToCompound(cc)

  
  case class Variable(variableName:Symbol) {
    def :=(value:Value) : Unit= {
      if((variableMap contains variableName) || (functionMap contains variableName)) {
        println(variableName)
        throw new Exception("Redefinition is not allowed!")
      }
      variableMap += (variableName -> simplify(value, variableMap))
    }
  }
    
  case class Function(val applier:Symbol) {
    def apply(arguments:Value*): Value  = {
      functionMap.get(applier) match {
        case Some(implementation) => implementation.getValueFromArguments(arguments)
        case None => {
          /* We could try and implement implied multiplication here */
          throw new Exception("We do not allow implied multiplication!")
        }
      }
    }
  }
  
  case class FunctionRegistration(functionName:Symbol) {
    case class Inner(parameters:Symbol*) {
      def :=(expression:Value) {
        for (parameter <- parameters) {
          if((variableMap contains functionName) || (functionMap contains functionName))
            throw new Exception("Redefinition is now allowed!")
        }
        ensureValueOnlyContainsUnboundWithSymbolicNames(expression, parameters)
        functionMap += (functionName -> new FunctionImplementation(parameters, expression))
      }
    }
    def of(parameters:Symbol*) = Inner(parameters : _*)
  } 
  
  case class FunctionImplementation(val parameters:Seq[Symbol], val expression:Value) {
    def getValueFromArguments(values:Seq[Value]) : Value = {
      if (values.length != parameters.length)
        throw new Exception("Incorrect number of arguments")
      
      var bindings:Map[Symbol, Value] = Map()
      // First we need to evaluate the arguments
      for (i <- 0 to (parameters.length - 1)) {
        var value = values(i)
        var parameter = parameters{i}
        var argument : Value = value match {
          case nv:NumberValue => nv
          case Unbound(symbol) => variableLookupFromBinding(symbol, variableMap)
          case compound:Compound => simplify(Simplifier.getCompoundGivenBinding(compound,  variableMap), variableMap)
        }

        bindings += (parameter -> argument)
      }
      return expression match {
        case nv:NumberValue => nv
        case umbound:Unbound => variableLookupFromBinding(umbound.sym, bindings)
        case compound:Compound => simplify(Simplifier.getCompoundGivenBinding(compound, bindings), bindings)
      }
    }
  }

  def DERIVE(expr:Value, wrt:Symbol): Value = expr match {
    case NumberValue(_,_) => 0
    case Unbound(sym) => if (sym == wrt) 1 else 0
    case Compound(op, lhs, rhs) => op match {
      case "*" => Compound("+", Compound("*", DERIVE(lhs, wrt), rhs), Compound("*", lhs, DERIVE(rhs, wrt)))
      case "+"|"-" => Compound(op, DERIVE(lhs, wrt), DERIVE(rhs, wrt))
      case "/" => Compound("/", Compound("-", Compound("*", DERIVE(lhs,wrt), rhs), Compound("*", lhs, DERIVE(rhs,wrt))), Compound("^", rhs, NumberValue(2,1)))
      case "^" => rhs match {
        case nv:NumberValue => Compound("*", nv, Compound("^", lhs, nv - 1))
        case otherwise => throw new Exception("not implemented")
      }
    }
  }
  
  //***************************************************************************
  //* PRINTING:
  //***************************************************************************
  
  val operators : String = "+-*/^" //add more if needed later
  val precedence = Array(4,4,3,3,2) //let's all stick to https://en.wikipedia.org/wiki/Order_of_operations
  val precedenceMap = Map("+" -> precedence(0), "-" -> precedence(1), "*" -> precedence(2), "/" -> precedence(3), "^" -> precedence(4)) 

  //pretty print: parenthesis only when needed
  def pprint(value:Value):Unit = value match {
    case NumberValue(n,d) => if (d==1) println(n) else println(n+"/"+d)
    case Unbound(sym) => println(sym.toString().substring(1))
    case Compound(o,r,l) => pprinthelp(simplify(Compound(o,r,l),variableMap),false); println()
  }
  
  //approximate and pretty print: 
  //same as pretty print but approximates fractions and known variables such as e or pi
  def aprint(value:Value):Unit = {
    
  }
  private def pprinthelp(value:Value, approximate:Boolean):Unit = value match {
    case NumberValue(n,d) => { 
      if (!approximate) { if (d==1) print(n) else print(n+"/"+d) }
      else print(n.toDouble/d.toDouble) 
    }
    case Unbound(sym) => print(sym.toString().substring(1))
    case Compound(op,lhs,rhs) => {
      if (op.equals("-") && isNumberValue(lhs) && getNum(lhs) == 0) {
        print(op)
        if (isCompound(rhs)) {
          print("(")
          pprinthelp(rhs,approximate)
          print(")")
        }
        else 
          pprinthelp(rhs,approximate)
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
      pprinthelp(lhs,approximate)
      if (parlhs) print(")")
      print(" " + op + " ")
      if (parrhs) print("(")
      pprinthelp(rhs,approximate)
      if (parrhs) print(")")     
    } 
  }
  
  
  //verose print: more parenthesis 
  def vprint(value:Value):Unit = {
    
  }
  
  //PRINTLN syntax: PRINTLN(whatever)
  def PRINTLN(value: Value): Unit =  {
    PRINT(value)
    println()
  }
  
  //PRINT syntax: PRINT(whatever)
  def PRINT(value: Value): Unit = simplify(value, variableMap) match {
    case numberValue:NumberValue => printNumberValue(numberValue)
    case unbound:Unbound => printUnbound(unbound)
    case compound:Compound => printCompoundUsingFunction(compound, PRINT)
  }
  
  // PRINTLN_EVALUATE syntax: PRINTLN_EVALUATE(whatever).. and evaluates the whatever exactly
  def PRINTLN_EVALUATE(value: Value): Unit = value match {
    case NumberValue(n,d) => println(n+"/"+d)
    case Unbound(sym) => println(sym) 
    case compound:Compound => {
      PRINT(Simplifier.getCompoundGivenBinding(compound, variableMap))
      println
    }
  }
  
  // PRINTLN_APPROXIMATE syntax: PRINTLN(whatever).. and evaluates the whatever exactly
  def PRINTLN_APPROXIMATE(value: Value): Unit = value match {
    case NumberValue(n,d) => println(n.toDouble/d.toDouble)
    case Unbound(sym) => if (isknown(sym)) println(approx(sym)) else println(sym) 
    case compound:Compound => {
      PRINTLN(Simplifier.getCompoundGivenBinding(compound, variableMap)) //approximate fix
    }
  }
  
  def printWithUnevaluatedUnbounds(value:Value): Unit = value match {
    case numberValue:NumberValue => printNumberValue(numberValue)
    case unbound:Unbound => print(unbound.sym)
    case compound:Compound => printCompoundUsingFunction(compound, printWithUnevaluatedUnbounds(_))
  }
  
  // PRINTSTRING syntax: PRINTSTRING(myString: String)
  def PRINTSTRING(value : String): Unit = println(value)
  
  def printNumberValue(numberValue:NumberValue) : Unit = {
    if (numberValue.den == 1)
      print(numberValue.num)
    else
      print(numberValue.num + "/" + numberValue.den)
  }
  
  def printUnbound(unbound:Unbound) : Unit = {
    if (variableMap contains unbound.sym)
      PRINT(variableMap(unbound.sym))
    else if (functionMap contains unbound.sym)
      printFunction(unbound.sym, functionMap(unbound.sym))
    else
      print(unbound.sym)
  }
  
  def printCompoundUsingFunction(compound:Compound, function:(Value) => Unit) : Unit = {
    print("(")
    function(compound.lhs)
    print(" " + compound.op + " ")
    function(compound.rhs)
    print(")")
  }
  
  def printFunction(functionName:Symbol, functionImplementation:FunctionImplementation) : Unit = {
    print("Function " + functionName.toString() + " takes in " + functionImplementation.parameters)
    print(" and is defined as ")
    printWithUnevaluatedUnbounds(functionImplementation.expression)
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
  
  def debug_print(v:Value, depth:Int = 0):Unit = v match {
    case NumberValue(n,d) => print("NV("+n+","+d+")")
    case Compound(op, lhs, rhs) => {
      print("("+op+" ")
      debug_print(lhs, depth+3)
      println()
      var i = 0
      for (i <- 0 to depth+2) {
        print(" ")
      }
      debug_print(rhs, depth+3)
      print(")")
      if (depth == 0) println()
    }
    case Unbound(s) => print(s.toString)
  }

  // From StackOverflow, so that we can pattern-match BigInts
  object IntBig {
    def unapply(b: BigInt) = Option(b.toInt)
  }

  def simplify(v:Value, binding:Map[Symbol, Value]):Value = {
    Simplifier.simplifier(v:Value, binding:Map[Symbol, Value])
  }
  

  // Returns the LCM of a and b
  //def lcm(a:Int, b:Int):Int = a*b / gcd(a,b) 

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
  def ensureValueOnlyContainsUnboundWithSymbolicNames(value:Value, symbolicNames:Seq[Symbol]): Unit = {
    value match {
      case NumberValue(_,_) => return
      case Unbound(sym:Symbol) => {
        if(!(symbolicNames contains sym))
          throw new Exception("You can't have any variables in a function body other than the parameter. Parameters are " + symbolicNames.toString() + ", found " + sym.toString())
      }
      case Compound(_, lhs, rhs) => {
        ensureValueOnlyContainsUnboundWithSymbolicNames(lhs, symbolicNames)
        ensureValueOnlyContainsUnboundWithSymbolicNames(rhs, symbolicNames)
      }
    }
  }
  
  
  /**
   * Returns true iff the given compound is made purely of NumberValues.
   */
  def allNumberValues(compound: Value): Boolean = compound match {
    case NumberValue(_,_) => true
    case Unbound(_) => false
    case Compound(op,v1:Value,v2:Value) => allNumberValues(v1) && allNumberValues(v2)
  }

  
  /**
   * Returns true iff the given Value is of type NumberValue.
   */
  def isNumberValue(value: Value): Boolean = value match {
    case NumberValue(n,d) => true
    case otherwise => false
  }
  
  /**
   * Gets numerator out of fraction.
   */
  def getNum(value: Value): BigInt = value match {
    case NumberValue(n,d) => n
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  /**
   * Gets denominator out of fraction.
   */
  def getDen(value: Value): BigInt = value match {
    case NumberValue(n,d) => d
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  
  /**
   * Returns true iff the given Value is of type Compound.
   */
  def isCompound(value: Value): Boolean = value match {
    case c: Compound => true
    case otherwise => false
  }
  
  
  /**
   * Returns true iff the given Value is of type Unbound.
   */
  def isUnbound(value: Value): Boolean = value match {
    case u: Unbound => true
    case otherwise => false
  }
  
  /**
   * Gets symbol out of Unbound.
   */
  def getSym(value: Value): Symbol = value match {
    case Unbound(s) => s
    case otherwise => 'youwrong //your risk to call this on st that is not unbound
  }
}
