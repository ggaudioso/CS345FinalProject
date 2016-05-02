//  ***********************************************************************************************************************************
//
//
//    ******      ******        ****       ***************   ***       ***          ***
//    *** ***    *** ***      ***  ***     ***************   ***       ***          ***
//    ***  ***  ***  ***    ***      ***         ***         ***       ***          ***
//    ***   ******   ***   ***        ***        ***         *************          ***
//    ***    ****    ***   **************        ***         *************          ***
//    ***            ***   **************        ***         ***       ***        
//    ***            ***   ***        ***        ***         ***       ***          ***
//    ***            ***   ***        ***        ***         ***       ***          ***
//
//
//  MathCode.scala  : DSL for Scala implementing Math features 
//  MathDemo.scala  : Little Demonstration showing features
//  Simplifier.scala: helper of mathcode, simplifier of expression
//  Test.scala      : developers' tests
//  DOCUMENTATIO TO BE ADDED!
//  
//  Contributors: Josh Cannon, Mike Feilbach, Ginevra Gaudioso, Lane Kolbly,
//
//  Values:               1 OVER 4            fraction 1/4
//                        1.234               real number
//                        'x                  parameter x (unbound)
//                        'x +4*'b            expression of the above. Operators are: + (plus), - (both unary and binary minus), * (times), / (divide), ^ (exponent).
//                        'Infinity           infinity
//                        -'Infinity          -infinity
//
//  Variable Declaration: 'a := 'b + 3        same as a=b+3  (b does not have to be defined, can be a parameter)
//  Function Declaration: 'f of 'x := 'x + 1  same as f(x)=x+1
//                        'p of ('x, 'y) when ('x === 'y) := 0
//                        'p of ('x, 'y) when ()          := 'x * 'y
//                                            same as p(x,y) = 0 if x=y, x*y otherwise
//  Function Call:        'f('x)              calls stored function f with argument x
//                        'sin^'x             calls predefined function sin(x). List of predefined functions: sin, cos, tan, ln, log, exp, sqrt
//
//  Printing:             pprint('a)          prints the value associated with a in its simplified form, or just a if such value does not exist
//                        aprint('a)          prints an approximation of a (ie 3/4 = 0.75, 'pi = 3.1415, and so on)
//                        setprecision(numdigits)
//                                            sets the precision of aprint to numdigits after the dot
//
//  Solve one equation:   solve(lhs,rhs,unknown)
//                                            From lhs=rhs returns unknown=expression: solves equation lhs=rhs for variable unknown. Use as argument of pprint or aprint to print result
//  Solve of system of two equations in two unknowns:
//                        solve(lhs1,rhs1,lhs2,rhs2,wrt1,wrt2)
//                                            From { lhs1=rhs1 , lhs2=rhs2 } returns { wrt1=expr1 , wrt2=expr2 }. Use as argument of pprint to print result
//
//  Derive:               derive(expression, variable)
//                                            Returns the derivative of the expression with respect to the variable. Use as argument of pprint to print result
//  Integrate:            integrate(expression, variable)
//                                            Returns the indefinite integral of the expression with respect to the variable. Use as argument of pprint to print result
//                        integrate(expression, variable, a, b)
//                                            Returns the definite integral from a t b of the expression with respect to the variable. Use as argument of pprint or a print to print result
//  Limits:               limit(expr,variable,value)
//                                            Returns the limit of the expression as the variable approaches value. Use as argument of pprint or aprint to print result
//
//  Summations:           summation(expr,variable,from,to)
//                                            Returns summation of the expression when variable goes from "from" to "to". Use as argument of pprint or aprint to print result
//  Products:             product(expr,variable,from,to)
//                                            Returns product of the expression when variable goes from "from" to "to". Use as argument of pprint or aprint to print result
//  Factorial:            factorial(value)    Returns the factorial of the value. Use as argument of pprint or aprint to print result
//
// 
//  See full documentation PDF for a full list of features and more detailed explanations
//
//
//  ***********************************************************************************************************************************



import scala.language.implicitConversions
import scala.math.{ pow, min, log, abs, exp, sin, tan, sqrt, cos, rint }

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
    def equals(other:Any):Boolean
  }
  
 
  //***************************************************************************
  //* TYPES IN OUR LANGUAGE AND THEIR OPERATORS:
  //***************************************************************************

  // "numbers" - Rational, bigint, bigdecimal, whatever. But an actual, known number.
  // Namely, a known number that isn't irrational
  case class NumberValue(val num:BigInt, val den:BigInt) extends Value {
    def + (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => {
        simplify(NumberValue(num*den2 + num2*den,den*den2))
      }
      case Unbound(sym) => Compound("+", this, sym)
      case c:Compound => Compound("+", this, c)
    }
    def - (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2 - num2*den,den*den2))
      case otherwise => Compound("-", this, rhs)
    }
    def * (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num2*num, den2*den))
      case otherwise => Compound("*", this, rhs)
    }
    def / (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2, num2*den))
      case otherwise => Compound("/", this, rhs)
    }
    def ^ (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => {
        if (den2==1) {
          simplify(NumberValue(num.pow(num2.toInt), den.pow(den2.toInt)))
        } else
          Compound("^",NumberValue(num.pow(num2.toInt), den.pow(num2.toInt)), NumberValue(1,den2))
      }
      case otherwise => Compound("^", this, rhs)
    }
    def OVER (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2, den*num2))
      case otherwise => Compound("/", this, rhs)
    }

    override def equals(other:Any):Boolean = other match {
      case NumberValue(n2,d2) => ((num == n2) && (den == d2))
      case otherwise => false
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

    override def equals(other:Any):Boolean = other match {
      case Unbound(s) => sym == s
      case otherwise => false
    }
    
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

    override def equals(other:Any):Boolean = other match {
      case Compound(op2,lhs2,rhs2) => {
        println("Equal?")
        debug_print(lhs2)
        println
        debug_print(rhs2)
        println
        (op == op2) && (op2 match {
          case "+"|"*" => ((lhs2 == lhs) && (rhs2 == rhs)) || ((lhs2 == rhs) && (rhs2 == lhs))
          case otherwise => ((lhs == lhs2) && (rhs == rhs2))
        })
      }
      case otherwise => false
    }
    
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
    decimals = min(decimals, 5) 
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
  
  implicit def symbolToComparator(symbolName:Symbol):Comparator = Comparator(symbolName)
  implicit def valueToComparator(value:Value):Comparator = Comparator(value)
  
  //implicit def compoundClusterToCompound(cc:CompoundCluster):Compound = Simplifier.compoundClusterToCompound(cc)

  
  //***************************************************************************
  //* FUNCTIONS AND VARIABLES:
  //***************************************************************************  
  case class Variable(variableName:Symbol) {
    def :=(value:Value) : Unit= {
      if((variableMap contains variableName) || (functionMap contains variableName)) {
        throw new Exception("Redefinition is not allowed!")
      }
      else if (isknownfunction(Compound("^",variableName,'xxx))) {
        throw new Exception("variable name clashes with predefined function, not allowed")
      }
      variableMap += (variableName -> simplify(value, variableMap))
    }
  }
    
  case class Comparator(value:Value){
    def >  (rhs:Value) = Comparison(BooleanOperator.>,  value, rhs, Seq())
    def >= (rhs:Value) = Comparison(BooleanOperator.>=, value, rhs, Seq())
    def <  (rhs:Value) = Comparison(BooleanOperator.<,  value, rhs, Seq())
    def <= (rhs:Value) = Comparison(BooleanOperator.<=, value, rhs, Seq())
    def ===(rhs:Value) = Comparison(BooleanOperator.==, value, rhs, Seq())
    def !==(rhs:Value) = Comparison(BooleanOperator.!=, value, rhs, Seq())
  }

  object BooleanOperator {
    sealed trait EnumVal
    case object <  extends EnumVal
    case object <= extends EnumVal
    case object >  extends EnumVal
    case object >= extends EnumVal
    case object == extends EnumVal
    case object != extends EnumVal
  }

  case class Comparison(op:BooleanOperator.EnumVal, lhs:Value, rhs:Value, var parameters:Seq[Symbol]) {
    def isSatisfiedFromArguments(arguments:Value*) : Boolean = {
      // We need to bind the parameters to the arguments
      var bindings:Map[Symbol, Value] = getMappingFromParametersToArguments(parameters, arguments)
      
      var simplifiedLHS : Value = lhs match {
        case nv:NumberValue => nv
        case umbound:Unbound => variableLookupFromBinding(umbound.sym, bindings)
        case compound:Compound => simplify(gcgb(compound, bindings), bindings)
      }
      
      var simplifiedRHS : Value = rhs match {
        case nv:NumberValue => nv
        case umbound:Unbound => variableLookupFromBinding(umbound.sym, bindings)
        case compound:Compound => simplify(gcgb(compound, bindings), bindings)
      }
      
      var lhsAsNumberValue : NumberValue = simplifiedLHS match {
        case nv:NumberValue => nv
        case umbound:Unbound => throw new Exception("In order to evaluate piecewise functions, all arguments must be known values!")
        case compound:Compound => throw new Exception("In order to evaluate piecewise functions, all arguments must be known values!")
      }
      
      var rhsAsNumberValue : NumberValue = simplifiedRHS match {
        case nv:NumberValue => nv
        case umbound:Unbound => throw new Exception("In order to evaluate piecewise functions, all arguments must be known values!")
        case compound:Compound => throw new Exception("In order to evaluate piecewise functions, all arguments must be known values!")
      }
        
      var lhsNum = lhsAsNumberValue.num
      var lhsDen = lhsAsNumberValue.den
      var rhsNum = rhsAsNumberValue.num
      var rhsDen = rhsAsNumberValue.den
      
      return op match {
        case BooleanOperator.>  => return (lhsNum/lhsDen) >  (rhsNum/rhsDen)
        case BooleanOperator.>= => return (lhsNum/lhsDen) >= (rhsNum/rhsDen)
        case BooleanOperator.<  => return (lhsNum/lhsDen) <  (rhsNum/rhsDen)
        case BooleanOperator.<= => return (lhsNum/lhsDen) <= (rhsNum/rhsDen)
        case BooleanOperator.== => return (lhsNum/lhsDen) == (rhsNum/rhsDen)
        case BooleanOperator.!= => return (lhsNum/lhsDen) != (rhsNum/rhsDen)
      }
    }
    
    def ensureComparisonOnlyContains(symbolicNames:Seq[Symbol]) = {
      ensureValueOnlyContainsUnboundWithSymbolicNames(lhs, symbolicNames)
      ensureValueOnlyContainsUnboundWithSymbolicNames(rhs, symbolicNames)
    }
  }

case class Function(val applier:Symbol) {
    def apply(arguments:Value*): Value  = {
      // First see if it is a function
      functionMap.get(applier) match {
        case Some(implementation) => implementation.getValueFromArguments(arguments)
        case None => {
          // Then see if it is a piecewise function
          if(piecewiseFunctionMap contains applier) {
            // If it is a piecewise function, there are 2 options. Either this is a recursive function call, or it is a function call
            if(applier == currentPiecewiseName) {
              // It is a recursive call
              // We cheat by replacing the call itself with an unbound whose name is a UUID
              var recursiveSymbol = Symbol(java.util.UUID.randomUUID.toString)
              // We then keep track of it for later
              if (!(recursiveCallMap contains recursiveSymbol)) {
                recursiveCallMap += (recursiveSymbol -> (applier, arguments))
              }
              else
              {
                throw new Exception("Catastrophic failure!")
              }
              return Unbound(recursiveSymbol)
            }
            else {
              handlePiecewiseFunctionCall(applier, arguments:_*)
            }
          }
          else {
              /* We could try and implement implied multiplication here */
              throw new Exception("We do not allow implied multiplication!")
          }
        }
      }
    }
  }

  case class FunctionRegistration(functionName:Symbol) {
    case class Registrar(parameters:Symbol*) {
      def :=(expression:Value) {
        if((variableMap contains functionName) || (functionMap contains functionName))
          throw new Exception("Redefinition is not allowed!")
        else if (isknownfunction(Compound("^",functionName,'xxxx)))
          throw new Exception("Function name clashes with predefined function, not allowed")
        ensureValueOnlyContainsUnboundWithSymbolicNames(expression, parameters)
        functionMap += (functionName -> new FunctionImplementation(parameters, expression))
      }

      case class PieceRegistrar(comparisons:Comparison*) {
        for(comparison <- comparisons) {
          comparison.ensureComparisonOnlyContains(parameters)
          comparison.parameters = parameters
        }
        
        def :=(expression:Value) {
          if((variableMap contains functionName) || (functionMap contains functionName)) // don't check piecewise function map
            throw new Exception("Redefinition is now allowed!")
          ensureValueOnlyContainsUnboundWithSymbolicNames(expression, parameters)
          if(piecewiseFunctionMap contains functionName) {
            var functionImplementationSequence = piecewiseFunctionMap(functionName)
            
            if(functionImplementationSequence(0)._2.parameters != parameters) {
              throw new Exception("Each Piecewise function defintition must have the exact same parameters!")
            }
            
            piecewiseFunctionMap = piecewiseFunctionMap - functionName // take it out
            functionImplementationSequence = functionImplementationSequence :+ (comparisons, new FunctionImplementation(parameters, expression))
            piecewiseFunctionMap += (functionName -> functionImplementationSequence)
          }
          else {
            piecewiseFunctionMap += (functionName -> Seq((comparisons, new FunctionImplementation(parameters, expression))))
          }
          isInPiecewiseDefinition = false
          currentPiecewiseName = null
        }
      }

      def when (comparisons:Comparison*) = {
        isInPiecewiseDefinition = true
        currentPiecewiseName = functionName
        PieceRegistrar(comparisons:_*)
      }
    }

    def of(parameters:Symbol*) = Registrar(parameters : _*)
  }

  case class FunctionImplementation(val parameters:Seq[Symbol], val expression:Value) {
    def getValueFromArguments(values:Seq[Value]) : Value = {
      if (values.length != parameters.length)
        throw new Exception("Incorrect number of arguments")

      var bindings:Map[Symbol, Value] = getMappingFromParametersToArguments(parameters, values)
      return expression match {
        case nv:NumberValue => nv
        case umbound:Unbound => variableLookupFromBinding(umbound.sym, bindings)
        case compound:Compound => simplify(gcgb(compound, bindings), bindings)
      }
    }
  }
  
  
  //***************************************************************************
  //* INSTRUCTIONS:
  //***************************************************************************
  
  //derives an expression with respect to a variable
  def derive(expr:Value, wrt:Symbol): Value = {
    return simplify(DERIVE(expr:Value, wrt:Symbol))
  }
  private def DERIVE(expr:Value, wrt:Symbol): Value = expr match {
    case NumberValue(_,_) => 0
    case Unbound(sym) => if (sym == wrt) 1 else 0
    case Compound(op, lhs, rhs) => op match {
      case "*" => Compound("+", Compound("*", DERIVE(lhs, wrt), rhs), Compound("*", lhs, DERIVE(rhs, wrt)))
      case "+"|"-" => Compound(op, DERIVE(lhs, wrt), DERIVE(rhs, wrt))
      case "/" => Compound("/", Compound("-", Compound("*", DERIVE(lhs,wrt), rhs), Compound("*", lhs, DERIVE(rhs,wrt))), Compound("^", rhs, NumberValue(2,1)))
      case "^" =>  {
        if (ishere(lhs,wrt) && !ishere(rhs,wrt))
          Compound("*", DERIVE(lhs,wrt),Compound("*",rhs,Compound("^",lhs,Compound("-",rhs,1))))
        else if (!ishere(lhs,wrt) && !ishere(rhs,wrt))
          0
        else if (!ishere(lhs,wrt) && ishere(rhs,wrt)) {
          if (!isknownfunction(Compound(op, lhs, rhs)))
            Compound("*",Compound(op, lhs, rhs), Compound("*",DERIVE(rhs,wrt),Compound("^",'ln,lhs))) //ln^x means ln(x)
          else {
            var funder = Compound("+",0,0)
            getSym(lhs) match {
              case 'ln => funder = Compound("/",1,rhs)
              case 'log => funder = Compound("/",1, Compound("*",rhs,Compound("^",'ln,10)))
              case 'exp => funder = Compound(op, lhs, rhs)
              case 'sin => funder = Compound("^", 'cos, rhs)
              case 'cos => funder = Compound("-",0,Compound("^", 'sin, rhs))
              case 'tan => funder = Compound("+",1,Compound("^",Compound("^",'tan,rhs),2))
              case 'sqrt => funder = Compound("/",1,Compound("*",2,Compound("^",'sqrt,rhs)))
              case otherwise => throw new Exception("this should never happen")
            }
            return Compound("*", funder, DERIVE(rhs,wrt))
          }
        }
        else throw new Exception("derivative not implemented")
      }
    }
  }
  private def ishere(where:Value, what:Symbol): Boolean = where match {
    case NumberValue(n,d) => false
    case Unbound(sym) => what.equals(sym)
    case Compound(op,lhs,rhs) => {
      var left = ishere(lhs,what)
      var right = ishere(rhs,what)
      return (left||right)
    }
  }
  
  
    
  //integrates an expression with respect to a variable
  def integrate(expr:Value, wrt:Symbol): Value = {
    return simplify(INTEGRATE(expr:Value, wrt:Symbol))
  }
  private def INTEGRATE(expr:Value, wrt:Symbol): Compound = expr match {
    case NumberValue(n,d) => Compound("*", NumberValue(n,d),wrt)
    case Unbound(sym) => { 
      if (sym == wrt) Compound("/",Compound("^",wrt,2),2)
      else Compound("*",Unbound(sym),wrt)
    }
    case Compound(op, lhs, rhs) => op match {
      case "*" => {
        if (ishere(lhs,wrt) && !ishere(rhs,wrt)) //rhs constant
          Compound("*",rhs,INTEGRATE(lhs,wrt))
        else if (ishere(rhs,wrt) && !ishere(lhs,wrt)) //lhs constant
          Compound("*",lhs,INTEGRATE(rhs,wrt))
        else if (!ishere(rhs,wrt) && !ishere(lhs,wrt)) //all constant
          Compound("*",Compound("*",lhs,rhs),wrt)
        else 
          throw new Exception("product not implemented")
      }
      case "+"|"-" => Compound(op, INTEGRATE(lhs, wrt), INTEGRATE(rhs, wrt))
      case "/" => {
        if (ishere(lhs,wrt) && !ishere(rhs,wrt)) //rhs constant
          Compound("/",INTEGRATE(lhs,wrt),rhs)
        else if (ishere(rhs,wrt) && !ishere(lhs,wrt)) //lhs constant
          throw new Exception("variable in denumerator not implemented")
        else if (!ishere(rhs,wrt) && !ishere(lhs,wrt)) //all constant
          Compound("*",Compound("*",lhs,rhs),wrt)
        else 
          throw new Exception("division not implemented")
      }
      case "^" =>  {
        if (ishere(lhs,wrt) && !ishere(rhs,wrt))
          Compound("/", Compound("/",Compound("^",lhs,Compound("+",rhs,1)),Compound("+",rhs,1)) ,DERIVE(lhs,wrt))
        else if (!ishere(lhs,wrt) && !ishere(rhs,wrt))
          Compound("*",Compound(op, lhs, rhs),wrt)
        else if (isknownfunction(Compound(op, lhs, rhs))) {
            var funint = Compound("+",0,0)
            getSym(lhs) match {
              case 'ln => funint = Compound("*",rhs,Compound("-",Compound(op,lhs,rhs),1))
              case 'log => funint = Compound("/",Compound("*",rhs,Compound("-",Compound(op,lhs,rhs),1)),Compound("^",'ln,10))
              case 'exp => funint = Compound(op, lhs, rhs)
              case 'sin => funint = Compound("-",0,Compound("^", 'cos, rhs))
              case 'cos => funint = Compound("^", 'sin, rhs)
              case 'tan => funint = Compound("-",0,Compound("^",'ln,Compound("^",'cos,rhs)))
              case 'sqrt => funint = Compound("/",Compound("*",2,Compound("^",rhs,1.5)),3)
              case otherwise => throw new Exception("this should never happen")
            }
            return Compound("/", funint, DERIVE(rhs,wrt))
        }
        else throw new Exception("integral not implemented")
      }
    }
  }
  
  //solves definite integral
  def integrate(expr:Value,wrt:Symbol,a:Value,b:Value):Value = {
    if (isNumberValue(a) && isNumberValue(b))
      intapprox(expr,wrt,getNum(a).toDouble/getDen(a).toDouble,getNum(b).toDouble/getDen(b).toDouble)
    else {
      var indefinite = INTEGRATE(expr,wrt) //might throw exception, oh well
      pprint(indefinite)
      var flaga, flagb = false
      var defa,defb = Compound("+",0,0)
      if (isUnbound(b) && getSym(b).equals('Infinity)) {
        defb = gcgb(indefinite,Map(wrt->'binf))
        flagb = true
      }
      else 
       defb = gcgb(indefinite,Map(wrt->b))
      if (isCompound(a) && getOp(a).equals("-") 
          && isNumberValue(getLhs(a)) && getNum(getLhs(a))==0 
          && isUnbound(getRhs(a)) && getSym(getRhs(a)).equals('Infinity) ) { 
        defa = gcgb(indefinite,Map(wrt->'ainf))
        flaga = true
      }
      else 
        defa = gcgb(indefinite,Map(wrt->a))
      if (!flaga && !flagb) 
        return Compound("-",defb,defa)
      else if (flagb && !flaga) {
        var lim = limit(Compound("-",defb,defa),'binf,'Infinity)
        if (isUnbound(lim) && getSym(lim).equals('Infinity)) return 'Infinity
        if (isCompound(lim) && getOp(lim).equals("-") && isNumberValue(getLhs(lim)) && getNum(getLhs(lim))==0
             && isUnbound(getRhs(lim)) && getSym(getRhs(lim)).equals('Infinity))
           return -'Infinity
        else return approx(lim,'rrrrrrrrrr,0)
      }
      else if(flaga && !flagb) {
        var lim = limit(Compound("-",defb,defa),'ainf,-'Infinity)
         if (isUnbound(lim) && getSym(lim).equals('Infinity)) return 'Infinity
         if (isCompound(lim) && getOp(lim).equals("-") && isNumberValue(getLhs(lim)) && getNum(getLhs(lim))==0
             && isUnbound(getRhs(lim)) && getSym(getRhs(lim)).equals('Infinity))
           return -'Infinity
         else return approx(lim,'rrrrrrrrr,0)
      }
      else {
        return integrate(expr,wrt,-'Infinity,0) + integrate(expr,wrt,0,'Infinity) //chop it
      }
    }
  }
  //approximation using trapezoids
  private def intapprox(expr:Value,wrt:Symbol,a:Double,b:Double):Value = {
    var fun = Compound("+",expr,0) //need compound later
    var num = 10  
    var heigths = 0.0
    var delta = (b-a)/num.toDouble
    for (i <- 0 to num) {
      var x = a + delta*i
      var y = approx(fun,wrt,x)
      if (i!=0 && i!=num) y=y*2
      heigths += y
    }
    return heigths*delta/2
  }
  private def approx(what:Value,wrt:Symbol,x:Double):Double = what match {
    case NumberValue(n,d) => return n.toDouble/d.toDouble
    case Unbound(sym) => { 
      if (sym==wrt) return x
      else if (knownVariables.contains(sym))
        return knownVariables(sym)
      else { pprint(sym); throw new Exception("cannot approximate variables") }
    }
    case Compound(o,l,r) => {
      if (isknownfunction(Compound(o,l,r))) 
        approxknownfunction(gcgb(Compound(o,l,r),Map(wrt->x)))
      else o match {
        case "+" => return approx(l,wrt,x) + approx(r,wrt,x)
        case "-" => return approx(l,wrt,x) - approx(r,wrt,x)
        case "*" => return approx(l,wrt,x) * approx(r,wrt,x)
        case "/" => return approx(l,wrt,x) / approx(r,wrt,x)
        case "^" => return pow(approx(l,wrt,x),approx(r,wrt,x))
        case otherwise => throw new Exception("unknown operator")
      }
    }
  }
  
  
  //solves lhs=rhs to return wrt = expression (pulls out wrt)
  //returns value of wrt
  def solve(lhs:Value,rhs:Value,wrt:Symbol):Value = {
    var lhs_rhs = group(simplify(lhs-rhs),wrt)
    if (!ishere(lhs_rhs,wrt)) return 'Undefined
    if (!ishereOnce(lhs_rhs,wrt)) throw new Exception("cannot solve equation, try moving things around")
    else  //can solve
        return simplify(solver(lhs_rhs,0,wrt))
  }
  
  //solves system of equations lhs1=rhs1 && lhs2=rhs2 with respect of the unknowns wrt1 and wrt2
  //returns list of values of wrt1 and wrt2. 
  def solve(lhs1:Value,rhs1:Value,lhs2:Value,rhs2:Value,wrt1:Symbol,wrt2:Symbol):(Value,Value) = {
    var wrt1solved1 = solve(lhs1,rhs1,wrt1)
    var wrt1solved2 = solve(lhs2,rhs2,wrt1)
    var wrt2solution = solve(wrt1solved1,wrt1solved2,wrt2)
    var wrt1solution = simplify(gcgb(Compound("+",wrt1solved1,0),Map(wrt2->wrt2solution)))
    return (wrt1solution,wrt2solution)
  }
  
  //helpers of solve methods:
  private def ishereOnce(where:Value, what:Symbol): Boolean = where match {
    case NumberValue(n,d) => false
    case Unbound(sym) => what.equals(sym)
    case Compound(op,lhs,rhs) => {
      var left = ishereOnce(lhs,what)
      var right = ishereOnce(rhs,what)
      return (left||right) && !(left&&right)  //exclusive or
    }
  }
  private def group(cc:Value,wrt:Symbol):Compound = {
    var c = Compound("+",cc,0)
    var coeff = simplify(gcgb(c,Map(wrt->1))-gcgb(c,Map(wrt->0)))
    var rest = simplify(gcgb(c,Map(wrt->0)))
    Compound("+",Compound("*",coeff,wrt),rest)
  } 
  private var opposites = Map("+"->"-","-"->"+","*"->"/","/"->"*") 
  private def solver(lhs:Value,rhs:Value,wrt:Symbol):Value = {
    //here I know that my variable is in the lhs, for how this helper is called
    //also, lhs must be a compound at this point
    var solved = rhs
    var stop = false
    var down = lhs match {
      case Compound(o,l,r) => Compound(o,l,r)
      case otherwise => throw new Exception("issue 1 in solve")
    }
    while (!stop) {
      var ll = down.lhs
      var rr = down.rhs
      var oo = down.op
      if (ishereOnce(ll,wrt)) {
        solved = Compound(opposites(oo), solved,rr)
        ll match {
          case Compound(o1,l1,r1) => down = Compound(o1,l1,r1)
          case Unbound(sym) => stop = true
          case otherwise => throw new Exception("issue 2 in solve")
        }
      }
      else if (ishereOnce(rr,wrt)) {
        oo match {
          case "+" | "*" => solved = Compound(opposites(oo),solved, ll) //commutative operators
          case otherwise => solved = Compound(oo,ll,solved)  //non commutative operators
        }
        rr match {
          case Compound(o1,l1,r1) => down = Compound(o1,l1,r1)
          case Unbound(sym) => stop = true
          case otherwise => throw new Exception("issue 3 in solve")
        }
      }
      else 
        throw new Exception("issue 4 in solve")
    }
    return solved   
  }
  
  
  //takes the limit of "expr" as "wrt" goes to "where"
  def limit(expr:Value,wrt:Symbol,where:Value): Value =  { 
    simplify(expr) match {
      case NumberValue(n,d) => NumberValue(n,d)
      case Unbound(sym) => if (wrt==sym) where else sym
      case Compound(o,l,r) => where match {
        case Compound(oo,ll,rr) => {
          if (oo.equals("-") && isNumberValue(ll) && getNum(ll)==0 && isUnbound(rr) && getSym(rr).equals('Infinity)) {
            var negwrt = Compound("-",0,wrt)
            var newcompound = gcgb(Compound(o,l,r), Map(wrt->negwrt))
            return limit(newcompound,wrt,'Infinity)
          }
          throw new Exception("limit cannot go to expression")
        }
        case NumberValue(n,d) => gcgb(Compound(o,l,r), Map(wrt->NumberValue(n,d)))
        case Unbound(sym) => { 
          if (sym=='Infinity) {
            if (!ishere(expr,wrt)) expr 
            else gotoinf(Compound(o,l,r),wrt)
          }
          else gcgb(Compound(o,l,r), Map(wrt->sym))
        }
      }
    }
  }
  //helper for limit
  private def gotoinf(expr:Compound,wrt:Symbol):Value = { 
    //Uses the definition of limit. 
    //If the expression stabilizes around a value as wrt increases, then the limit is that value
    //If the expression keeps growing as wrt increases, then the limit is infinity
    var epsilon = 0.00001
    var startx = 100
    var starty = simplify(gcgb(expr,Map(wrt->startx)))
    var difference:Value = 0
    var difference1:Value = 0
    for (i <- 1 to 100) {
      var x =  startx + 1000*i
      var y = simplify(gcgb(expr,Map(wrt->x)))
      difference = simplify(Compound("-",y,starty))
      var diff = 0.0
      //if (!isNumberValue(difference)) 
        diff = approx(difference,'rrrrrrrrrrrr,0) //this will throw exception if it has to
      if (abs(diff)<=epsilon)  return y 
      starty = y
      difference1 = difference
    }
    var diff = 0.0
    var diff1 = 0.0
    if (!isNumberValue(difference))
      diff = approx(difference,'rrrrrrrrrrrr,0) //this will throw exception if it has to
    if (!isNumberValue(difference1)) 
      diff1 = approx(difference,'rrrrrrrrrrrr,0) //this will throw exception if it has to
    if (diff1*diff < 0) return 'Oscillating //hardly ever finds this, but whatever.
    else if (diff>0) return 'Infinity
    else return Compound("-",0,'Infinity)
  }
  
  //checks if symbol^something actually is a known function
  //here defined all the known functions, also
  private def isknownfunction(what:Compound):Boolean = {
    if (!what.op.equals("^")) false
    else if (!isUnbound(what.lhs)) false
    else {
      var name = getSym(what.lhs)
      if (name.equals('ln) || name.equals('log) || name.equals('exp)   //natural log, base 10 log, e to the
          || name.equals('sin) || name.equals('cos) || name.equals('tan) //sine, cosine, tangent
          || name.equals('sqrt))  //square root
         //can add more
        true
      else false
    }
  }
  //returns approximation using known function, if applied to a number
  private def approxknownfunction(what:Compound):Double =  {
    if (!isknownfunction(what)) { pprint(what); throw new Exception(".. this is not a known function") }
    var arg = approx(what.rhs,'rrrrrrrrr,0) //this will throw an exception if it has to
    getSym(what.lhs) match {
      case 'ln => log(arg)
      case 'log => log(arg)/log(10)
      case 'exp => exp(arg)
      case 'sin => sin(arg)
      case 'cos => cos(arg)
      case 'tan => tan(arg)
      case 'sqrt => sqrt(arg)
    }
  }
  
  //takes summation of expression with variable wrt going from a to b
  def summation(expr:Value,wrt:Symbol,a:Value,b:Value):Value = {
    dosumprod(expr,wrt,a,b,true)
  }
  
  //takes product of expression with variable wrt going from a to b
  def product(expr:Value,wrt:Symbol,a:Value,b:Value):Value = {
    dosumprod(expr,wrt,a,b,false)
  }
  
  //helpers for summation and product
  private def dosumprod(expr:Value,wrt:Symbol,a:Value,b:Value,issum:Boolean):Value = {
      var compexpr = Compound("+",expr,0)
    if (isNumberValue(a) && isNumberValue(b)) {
      if (getDen(a)!=1 || getDen(b)!=1) throw new Exception("Summation is only defined with integer bounds")
      else {
        var aint = getNum(a).toInt
        var bint = getNum(b).toInt
        var sumorprod:Value = if (issum) Compound("+",0,0) else Compound("+",1,0)
        for (i <- aint to bint) {
          if (issum)
            sumorprod += gcgb(compexpr, Map(wrt->i))
          else 
            sumorprod *= gcgb(compexpr, Map(wrt->i))
        }
        return simplify(sumorprod)
      }
    }
    else if (isUnbound(b) && getSym(b).equals('Infinity) && isNumberValue(a)) {
      sumtoinf(expr,wrt,getNum(a).toInt,issum)
    }
    else
      throw new Exception("summation not implemented")
  }
  private def sumtoinf(expr:Value,wrt:Symbol,aa:Int,issum:Boolean):Value = {
    var epsilon = 0.000001
    var compexpr = Compound("+",expr,0)
    var a:Value = gcgb(compexpr, Map(wrt->aa))
    var sumorprod:Value = a
    var b:Value = a
    var start = aa+1
    for (i <- start to 1000) {
      a=b
      b = gcgb(compexpr, Map(wrt->i))
      var diff = abs(approx(simplify(b-a),'rrrrrrrrr,0))
      if (diff<epsilon) return sumorprod
      if (issum)
        sumorprod += b
      else 
        sumorprod *= b
    }
    var diff = approx(simplify(b-a),'rrrrrrrrr,0)
    if (diff<epsilon) return sumorprod
    else return 'Infinity
  }
  
  //returns the factorial of the argument
  def factorial(expr:Value):Value = simplify(expr) match {
    case NumberValue(n,d) => {
      if (d!=1) throw new Exception("factorial is only defined on integers")
      else if (n==0)  1
      else product('i,'i,1,n.toInt)
    }
    case Unbound(sym) => {
      if (variableMap contains sym) factorial(variableMap(sym))
      else throw new Exception("factorial only defined on known variables")
    }
    case otherwise => throw new Exception("factorial is only defined on integers")
  }
  
  
  
  //***************************************************************************
  //* PRINTING:
  //***************************************************************************
  
  def setprecision(digits:Int):Unit = { numdigits=pow(10,digits).toInt }
  
  //extended pprint for tuples
  def pprint(x:(Value,Value)):Unit = {
   print("(")
   pprinthelp(x._1,false)
   print(",")
   pprinthelp(x._2,false)
   println(")")
  }
  
  //pretty print: parenthesis only when needed
  def pprint(value:Value):Unit = value match {
    case NumberValue(n,d) => {
      var leastTerms = simplify(NumberValue(n,d))
      var nn = getNum(leastTerms)
      var dd = getDen(leastTerms)
      if (dd==1) println(nn) 
      else println(nn+"/"+dd)
    }
    case Unbound(sym) => { 
      if (variableMap contains sym) pprint(variableMap(sym)) 
      else if ( (functionMap contains sym) || (piecewiseFunctionMap contains sym)) vprint(sym)
      else println((sym.toString).substring(1))
    }
    case Compound(o,r,l) => {
      var bounded = gcgb(Compound(o,r,l), variableMap)
      pprinthelp(simplify(bounded,variableMap),false)
      println
    }
  }
  
  //approximate and pretty print: 
  //same as pretty print but approximates fractions and known variables such as e or pi
  def aprint(value:Value):Unit = value match {
    case NumberValue(n,d) => { 
      if (d!= 0) println((rint(n.toDouble/d.toDouble* numdigits))/numdigits) 
      else if (n==0) println("Undefined")
      else println("Infinity")
    }
    case Unbound(sym) => { 
      if (variableMap contains sym) pprinthelp((variableMap(sym)),true)
      else if (knownVariables contains sym) { 
        pprinthelp(knownVariables(sym),true)
        println
      }
      else println((sym.toString).substring(1))
    }
    case Compound(o,r,l) => {
      var bounded1 = gcgb(Compound(o,r,l),variableMap)
      var bounded2 = gcgb(bounded1,knownVariablesValue)
      pprinthelp(simplify(bounded2,variableMap),true) 
      println  
    }
  }
   
  private var numdigits = 100000 //number of zeros here = number of decimal places in print
                                 //numdigits = 1 -> prints closest int
                                 //numdigits = 100000 -> prints 5 digits after .
  
  //pprint helpers
  private val operators : String = "+-*/^" //add more if needed later
  private val precedence = Array(4,4,3,3,2) //let's all stick to https://en.wikipedia.org/wiki/Order_of_operations
  val precedenceMap = Map("+" -> precedence(0), "-" -> precedence(1), "*" -> precedence(2), "/" -> precedence(3), "^" -> precedence(4)) 
  
  private def pprinthelp(value:Value, approximate:Boolean):Unit = value match {
    case NumberValue(n,d) => { 
      if (!approximate) { if (d==1) print(n) else print(n+"/"+d) }
      else {
        if (n==0 && d==0) print("Undefined")
        else print((rint(n.toDouble/d.toDouble* numdigits))/numdigits) 
      }
    }
    case Unbound(sym) => { 
      if (variableMap contains sym) pprinthelp(variableMap(sym),approximate) 
      else if ( (functionMap contains sym) || (piecewiseFunctionMap contains sym)) vprint(sym)
      else print((sym.toString).substring(1))
    }
    case Compound(op,lhs,rhs) => {
      if (op.equals("-") && isNumberValue(lhs) && getNum(lhs) == 0) {
        print(op)
        rhs match {
          case Compound(oo,ll,rr) => {
            if (!isknownfunction(Compound(oo,ll,rr))) {
              print("(")
              pprinthelp(rhs,approximate)
              print(")")
            }
            else pprinthelp(rhs,approximate)
          }
          case otherwise => pprinthelp(rhs,approximate)
        }
        return
      }
      if (isknownfunction(Compound(op,lhs,rhs))) {
        if (approximate && isNumberValue(rhs)) 
          print((rint(approxknownfunction(Compound(op,lhs,rhs))*numdigits))/numdigits)
        else {
          pprinthelp(simplify(lhs), approximate)
          print("(")
          pprinthelp(simplify(rhs),approximate)
          print(")")
        }
        return
      }
      if (op.equals("/") && isNumberValue(rhs) && getNum(rhs)==getDen(rhs)) {
        pprinthelp(lhs,approximate)
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
      pprinthelp(simplify(lhs),approximate)
      if (parlhs) print(")")
      print(" " + op + " ")
      if (parrhs) print("(")
      pprinthelp(simplify(rhs),approximate)
      if (parrhs) print(")")     
    } 
  }
  
  //verbose print: more parenthesis 
  def vprint(value:Value):Unit = value match {
    case numberValue:NumberValue => printNumberValue(numberValue); println
    case unbound:Unbound => printUnbound(unbound); println
    case compound:Compound => printCompoundUsingFunction(compound, vprinthelp); println
  }
  
  //vprint helpers
  private def vprinthelp(value: Value): Unit = simplify(value, variableMap) match {
    case numberValue:NumberValue => printNumberValue(numberValue)
    case unbound:Unbound => printUnbound(unbound)
    case compound:Compound => printCompoundUsingFunction(compound, vprinthelp)
  }

  private def printWithUnevaluatedUnbounds(value:Value): Unit = value match {
    case numberValue:NumberValue => printNumberValue(numberValue)
    case unbound:Unbound => print(unbound.sym)
    case compound:Compound => printCompoundUsingFunction(compound, printWithUnevaluatedUnbounds(_))
  }
  
  private def printNumberValue(numberValue:NumberValue) : Unit = {
    if (numberValue.den == 1) print(numberValue.num)
    else print(numberValue.num + "/" + numberValue.den)
  }
  
  private def printUnbound(unbound:Unbound) : Unit = {
    if (variableMap contains unbound.sym)  vprinthelp(variableMap(unbound.sym))
    else if (functionMap contains unbound.sym) printFunction(unbound.sym, functionMap(unbound.sym))
    else print(unbound.sym)
  }
  
  private def printCompoundUsingFunction(compound:Compound, function:(Value) => Unit) : Unit = {
    print("(")
    function(compound.lhs)
    print(" " + compound.op + " ")
    function(compound.rhs)
    print(")")
  }
  
  private def printFunction(functionName:Symbol, functionImplementation:FunctionImplementation) : Unit = {
    print("Function " + functionName.toString() + " takes in " + functionImplementation.parameters)
    print(" and is defined as ")
    printWithUnevaluatedUnbounds(functionImplementation.expression)
  }
 
  
 //*****************************************************************
 //* STUFF TO DEAL WITH VARIABLES:
 //*****************************************************************  
  
  // Here is part of our rcursion hack. We keep track of state through the global variables to know if we're currently in a piecewise function
  // definition.
  private var isInPiecewiseDefinition = false;
  private var currentPiecewiseName : Symbol = null;
  
  //bindings of variables stored here:
  private var variableMap:Map[Symbol,Value] = Map()
  private var functionMap:Map[Symbol, FunctionImplementation] = Map()
  private var piecewiseFunctionMap:Map[Symbol, Seq[(Seq[Comparison], FunctionImplementation)]] = Map()
  private var recursiveCallMap : Map[Symbol, (Symbol, Seq[Value])] = Map()
  
  //known values such as pi, e .. can add more
  //if we increase precision, increase precision of these as well.. but not too much or operations with lots of these wil overflow and mess up
  private var knownVariables:Map[Symbol,Double] = Map(('e,2.7182818), ('pi,3.14159265))
  private var knownVariablesValue:Map[Symbol,Value] = Map(('e,2.7182818), ('pi,3.14159265)) //so Scala is happy everywhere
  
  //look up a variable given the binding
  private def variableLookupFromBinding(sym:Symbol, binding:Map[Symbol, Value]):Value = {
    binding.get(sym) match {
      case Some(value) => value
      case None => {
        // (We also need to look into the recursive function call map if there isnt a match)
        if(recursiveCallMap contains sym) {
          var funcAndArgs = recursiveCallMap(sym)
          var functionName = funcAndArgs._1
          var arguments = funcAndArgs._2
          for (index <- 0 until arguments.length){
            var newArg = arguments(index) match {
              case nv:NumberValue => nv
              case umbound:Unbound => variableLookupFromBinding(umbound.sym, binding)
              case compound:Compound => simplify(gcgb(compound, binding), binding) 
            }
            arguments = arguments.updated(index, newArg)
          }
          handlePiecewiseFunctionCall(functionName, arguments:_*)
        }
        else {
          Unbound(sym)
        }
      }
    }
  }
  
  //***************************************************************************
  //* HELPER METHODS.
  //***************************************************************************
  
  private def debug_print(v:Value, depth:Int = 0):Unit = v match {
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

  private def simplify(v:Value, binding:Map[Symbol, Value] = variableMap):Value = 
    Simplifier.simplifier(v:Value, binding:Map[Symbol, Value])
    
  /**
   * Given a Compound and a Binding, return a new Compound in which all unbound
   * variables are replaced by their bindings, if such a binding
   * exists.
   */
  def gcgb(c:Compound,binding:Map[Symbol, Value]):Compound = {
    // The final new lhs and rhs for this compound. These are
    // built recursively.
    var newLhs: Value = null
    var newRhs: Value = null
    var op: String = c.op
    
    newLhs = c.lhs match {
      case compound:Compound => gcgb(compound, binding)
      case numberValue:NumberValue => numberValue
      case Unbound(unboundSymbol) => variableLookupFromBinding(unboundSymbol, binding)
    }
    
    newRhs = c.rhs match {
      case compound:Compound => gcgb(compound, binding)
      case numberValue:NumberValue => numberValue
      case Unbound(unboundSymbol) => variableLookupFromBinding(unboundSymbol, binding)
    }
    
    return Compound(op, newLhs, newRhs) 
  }
    

  // Returns the LCM of a and b
  //def lcm(a:Int, b:Int):Int = a*b / gcd(a,b) 
  
  // For use in function bodies/Comparisons, to make sure there isn't anything we don't expect
  private def ensureValueOnlyContainsUnboundWithSymbolicNames(value:Value, symbolicNames:Seq[Symbol]): Unit = {
    value match {
      case NumberValue(_,_) => return
      case Unbound(sym:Symbol) => {
        if(!(symbolicNames contains sym) && !(recursiveCallMap contains sym)){ // This matters because it could be a recursive call
          throw new Exception("Out of scope variable detected. Valid variables are " + symbolicNames.toString() + ", found " + sym.toString())
        }
      }
      case Compound(_, lhs, rhs) => {
        ensureValueOnlyContainsUnboundWithSymbolicNames(lhs, symbolicNames)
        ensureValueOnlyContainsUnboundWithSymbolicNames(rhs, symbolicNames)
      }
    }
  }
  
  private def getMappingFromParametersToArguments(parameters:Seq[Symbol], arguments:Seq[Value]) : Map[Symbol, Value] = {
    assert(arguments.length == parameters.length)

    var bindings:Map[Symbol, Value] = Map()
    // First we need to evaluate the arguments
    for (i <- 0 to (parameters.length - 1)) {
      var unsimplifiedArgument = arguments(i)
      var parameter = parameters{i}
      var simplifiedArgument : Value = unsimplifiedArgument match {
        case nv:NumberValue => nv
        case Unbound(symbol) => variableLookupFromBinding(symbol, variableMap)
        case compound:Compound => simplify(gcgb(compound, variableMap), variableMap)
      }

      bindings += (parameter -> simplifiedArgument)
    }
    return bindings
  }
  
  private def handlePiecewiseFunctionCall(functionName:Symbol, arguments:Value*) : Value= {
    var sequence = piecewiseFunctionMap(functionName)
    // Loop through all the comparisons until one is satisfied
    for((comparisons, implementation) <- sequence) {
      var satisfied : Boolean = true
      for (comparison <-comparisons){
        if(!comparison.isSatisfiedFromArguments(arguments:_*)){
          satisfied = false
        }
      }
      if (satisfied)
        return implementation.getValueFromArguments(arguments)
    }
    throw new Exception("The supplied argument did not satisfy any of the piecewise requirements for the function!")
  }
  
  /**
   * Returns true iff the given compound is made purely of NumberValues.
   */
  private def allNumberValues(compound: Value): Boolean = compound match {
    case NumberValue(_,_) => true
    case Unbound(_) => false
    case Compound(op,v1:Value,v2:Value) => allNumberValues(v1) && allNumberValues(v2)
  }

  
  /**
   * Returns true iff the given Value is of type NumberValue.
   */
  private def isNumberValue(value: Value): Boolean = value match {
    case NumberValue(n,d) => true
    case otherwise => false
  }
  
  /**
   * Gets numerator out of fraction.
   */
  private def getNum(value: Value): BigInt = value match {
    case NumberValue(n,d) => n
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  /**
   * Gets denominator out of fraction.
   */
  private def getDen(value: Value): BigInt = value match {
    case NumberValue(n,d) => d
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  
  /**
   * Returns true iff the given Value is of type Compound.
   */
  private def isCompound(value: Value): Boolean = value match {
    case c: Compound => true
    case otherwise => false
  }
  
  private def getOp(value:Value):String = value match {
    case Compound(o,_,_) => o
    case otherwise => throw new Exception("get op called in something that is not a compound")
  }
  private def getRhs(value:Value):Value = value match {
    case Compound(_,_,l) =>l
    case otherwise => throw new Exception("get rhs called in something that is not a compound")
  }
  private def getLhs(value:Value):Value = value match {
    case Compound(_,r,_) =>r
    case otherwise => throw new Exception("get lhs called in something that is not a compound")
  }
  
  
  /**
   * Returns true iff the given Value is of type Unbound.
   */
  private def isUnbound(value: Value): Boolean = value match {
    case u: Unbound => true
    case otherwise => false
  }
  
  private def getSym(value: Value): Symbol = value match {
    case Unbound(sym) => sym
    case otherwise => throw new Exception("get sym called in something that is not a unbound")
  }

}
