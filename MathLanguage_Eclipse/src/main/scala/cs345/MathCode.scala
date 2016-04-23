import scala.language.implicitConversions
import scala.math.{ pow, min, log, abs }

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
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2 + num2*den,den*den2))
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
        if (den2==1) 
          simplify(NumberValue(num.pow(num2.toInt), den.pow(den2.toInt)))
        else
          Compound("^",NumberValue(num.pow(num2.toInt), den.pow(num2.toInt)), NumberValue(1,den2))
      }
      case otherwise => Compound("^", this, rhs)
    }
    def OVER (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2, den*num2))
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
  
  implicit def symbolToComparator(symbolName:Symbol):Comparator = Comparator(symbolName)
  implicit def valueToComparator(value:Value):Comparator = Comparator(value)
  
  //implicit def compoundClusterToCompound(cc:CompoundCluster):Compound = Simplifier.compoundClusterToCompound(cc)

  
  //***************************************************************************
  //* FUNCTIONS AND VARIABLES:
  //***************************************************************************  
  case class Variable(variableName:Symbol) {
    def :=(value:Value) : Unit= {
      if((variableMap contains variableName) || (functionMap contains variableName)) {
        println(variableName)
        throw new Exception("Redefinition is not allowed!")
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
        case compound:Compound => simplify(Simplifier.getCompoundGivenBinding(compound, bindings), bindings)
      }
      
      var simplifiedRHS : Value = rhs match {
        case nv:NumberValue => nv
        case umbound:Unbound => variableLookupFromBinding(umbound.sym, bindings)
        case compound:Compound => simplify(Simplifier.getCompoundGivenBinding(compound, bindings), bindings)
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
          piecewiseFunctionMap.get(applier) match {
            case Some(sequence) => {
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
            case None =>
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
        }
      }

      def when (comparisons:Comparison*) = PieceRegistrar(comparisons:_*)
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
        case compound:Compound => simplify(Simplifier.getCompoundGivenBinding(compound, bindings), bindings)
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
        else if (!ishere(lhs,wrt) && ishere(rhs,wrt))
          Compound("*",Compound(op, lhs, rhs), Compound("*",DERIVE(rhs,wrt),Compound("^",'ln,lhs))) //ln^x means ln(x)
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
      var bindb = Map(wrt->b)
      var defb = Simplifier.getCompoundGivenBinding(indefinite,bindb)
      var binda = Map(wrt->a)
      var defa = Simplifier.getCompoundGivenBinding(indefinite,binda)
      return Compound("-",defb,defa)
    }
  }
  //approximation using trapezoids
  private def intapprox(expr:Value,wrt:Symbol,a:Double,b:Double):Value = {
    var fun = Compound("+",expr,0) //need compound later
    var num = 10000  
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
    case Unbound(sym) => if (sym==wrt) return x else throw new Exception("cannot approximate variables")
    case Compound(o,l,r) => {
      o match {
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
  //assumes wrt appears once total
  def solve(lhs:Value,rhs:Value,wrt:Symbol):Value = {
    if (isUnbound(lhs) && getSym(lhs).equals(wrt)) return rhs
    if (isUnbound(rhs) && getSym(rhs).equals(wrt)) return lhs
    if (ishereOnce(lhs,wrt) == ishereOnce(rhs,wrt)) throw new Exception("Variable must appear exactly once")
    else { //can solve
       if (ishereOnce(lhs,wrt)) return solver(lhs,rhs,wrt)
       else return simplify(solver(rhs,lhs,wrt))
    }
  }
  private def ishereOnce(where:Value, what:Symbol): Boolean = where match {
    case NumberValue(n,d) => false
    case Unbound(sym) => what.equals(sym)
    case Compound(op,lhs,rhs) => {
      var left = ishereOnce(lhs,what)
      var right = ishereOnce(rhs,what)
      return (left||right) && !(left&&right)  //exclusive or
    }
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
  def limit(expr:Value,wrt:Symbol,where:Value): Value = simplify(expr) match {      
    case NumberValue(n,d) => NumberValue(n,d)
    case Unbound(sym) => if (wrt==sym) where else sym
    case Compound(o,l,r) => where match {
      case Compound(_,_,_) => throw new Exception("limit cannot go to expression")
      case NumberValue(n,d) => Simplifier.getCompoundGivenBinding(Compound(o,l,r), Map(wrt->NumberValue(n,d)))
      case Unbound(sym) => { 
        if (sym=='Infinity) { 
          if (!ishere(expr,wrt)) expr
          else gotoinf(Compound(o,l,r),wrt)
        }
        else Simplifier.getCompoundGivenBinding(Compound(o,l,r), Map(wrt->sym))
      }
    }
  }
  private def gotoinf(expr:Compound,wrt:Symbol):Value = { 
    //Uses the definition of limit. 
    //If the expression stabilizes around a value as wrt increases, then the limit is that value
    //If the expression keeps growing as wrt increases, then the limit is infinity
    var epsilon = 0.0001
    var startx = 100
    var starty = simplify(Simplifier.getCompoundGivenBinding(expr,Map(wrt->startx)))
    var difference:Value = 0
    for (i <- 1 to 10) {
      var x = startx + 1000*i
      var y = simplify(Simplifier.getCompoundGivenBinding(expr,Map(wrt->x)))
      difference = simplify(Compound("-",y,starty))
      difference match {
        case NumberValue(n,d) => {
          var diff = n.toDouble/d.toDouble
          if (abs(diff)<=epsilon) return y
        }
        case otherwise => throw new Exception("difference is not a simple number")
      }
      starty = y
    }
    difference match {
      case NumberValue(n,d) => {
        var diff = n.toDouble/d.toDouble
        if (diff>0) return 'Infinity
        else return Compound("-",0,'Infinity)
      }
      case otherwise => throw new Exception("difference is not a simple number")
    }
  }
  
  
  //***************************************************************************
  //* PRINTING:
  //***************************************************************************
  
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
      else println((sym.toString).substring(1))
    }
    case Compound(o,r,l) => {
      var bounded = Simplifier.getCompoundGivenBinding(Compound(o,r,l), variableMap)
      pprinthelp(simplify(bounded,variableMap),false)
      println
    }
  }
  
  //approximate and pretty print: 
  //same as pretty print but approximates fractions and known variables such as e or pi
  def aprint(value:Value):Unit = value match {
    case NumberValue(n,d) => { 
      if (d!= 0) println(n.toDouble/d.toDouble) 
      else if (n==0) println("Undefined")
      else println("Infinity")
    }
    case Unbound(sym) => { 
      if (variableMap contains sym) aprint(variableMap(sym))
      else if (knownVariables contains sym) aprint(knownVariables(sym))
      else println((sym.toString).substring(1))
    }
    case Compound(o,r,l) => {
      var bounded1 = Simplifier.getCompoundGivenBinding(Compound(o,r,l),variableMap)
      var bounded2 = Simplifier.getCompoundGivenBinding(bounded1,knownVariables)
      pprinthelp(simplify(bounded2,variableMap),true) 
      println  
    }
  }
  
  //pprint helpers
  private val operators : String = "+-*/^" //add more if needed later
  private val precedence = Array(4,4,3,3,2) //let's all stick to https://en.wikipedia.org/wiki/Order_of_operations
  val precedenceMap = Map("+" -> precedence(0), "-" -> precedence(1), "*" -> precedence(2), "/" -> precedence(3), "^" -> precedence(4)) 
  
  private def pprinthelp(value:Value, approximate:Boolean):Unit = value match {
    case NumberValue(n,d) => { 
      if (!approximate) { if (d==1) print(n) else print(n+"/"+d) }
      else {
        if (n==0 && d==0) print("Undefined")
        else print(n.toDouble/d.toDouble) 
      }
    }
    case Unbound(sym) => print(sym.toString().substring(1))
    /*case Compound("-",NumberValue(IntBig(0),_),rhs) => {
      print("-")
      rhs match {
        case c:Compound => {
          print("(")
          pprinthelp(rhs,approximate)
          print(")")
        }
        case otherwise => pprinthelp(rhs,approximate)
      }
    }*/
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
      if (op.equals("^") && isUnbound(lhs) && getSym(lhs)=='ln) {
        if (approximate && isNumberValue(rhs)) 
          print(log(getNum(rhs).toDouble/getDen(rhs).toDouble))
        else {
          print("ln(")
          pprinthelp(rhs,approximate)
          print(")")
        }
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
  
  
  //verose print: more parenthesis 
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
  
  //bindings of variables stored here:
  var variableMap:Map[Symbol,Value] = Map()
  var functionMap:Map[Symbol, FunctionImplementation] = Map()
  var piecewiseFunctionMap:Map[Symbol, Seq[(Seq[Comparison], FunctionImplementation)]] = Map()
  
  //known values such as pi, e .. can add more
  //if we increase precision, increase precision of these as well.. but not too much or operations with lots of these wil overflow and mess up
  var knownVariables:Map[Symbol,Value] = Map(('e,2.7182), ('pi,3.1415))
  
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

  def simplify(v:Value, binding:Map[Symbol, Value] = variableMap):Value = 
    Simplifier.simplifier(v:Value, binding:Map[Symbol, Value])
  
    

  // Returns the LCM of a and b
  //def lcm(a:Int, b:Int):Int = a*b / gcd(a,b) 
  
  // For use in function bodies/Comparisons, to make sure there isn't anything we don't expect
  def ensureValueOnlyContainsUnboundWithSymbolicNames(value:Value, symbolicNames:Seq[Symbol]): Unit = {
    value match {
      case NumberValue(_,_) => return
      case Unbound(sym:Symbol) => {
        if(!(symbolicNames contains sym))
          throw new Exception("Out of scope variable detected. Valid variables are " + symbolicNames.toString() + ", found " + sym.toString())
      }
      case Compound(_, lhs, rhs) => {
        ensureValueOnlyContainsUnboundWithSymbolicNames(lhs, symbolicNames)
        ensureValueOnlyContainsUnboundWithSymbolicNames(rhs, symbolicNames)
      }
    }
  }
  
  def getMappingFromParametersToArguments(parameters:Seq[Symbol], arguments:Seq[Value]) : Map[Symbol, Value] = {
    assert(arguments.length == parameters.length)

    var bindings:Map[Symbol, Value] = Map()
    // First we need to evaluate the arguments
    for (i <- 0 to (parameters.length - 1)) {
      var unsimplifiedArgument = arguments(i)
      var parameter = parameters{i}
      var simplifiedArgument : Value = unsimplifiedArgument match {
        case nv:NumberValue => nv
        case Unbound(symbol) => variableLookupFromBinding(symbol, variableMap)
        case compound:Compound => simplify(Simplifier.getCompoundGivenBinding(compound, variableMap), variableMap)
      }

      bindings += (parameter -> simplifiedArgument)
    }
    return bindings
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
  
  def getSym(value: Value): Symbol = value match {
    case Unbound(sym) => sym
    case otherwise => 'youwrong //your risk to call this on st that is not numbervalue
  }

}
