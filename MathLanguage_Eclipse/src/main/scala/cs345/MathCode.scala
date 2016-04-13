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
  }
  
 
  //***************************************************************************
  //* TYPES IN OUR LANGUAGE AND THEIR OPERATORS:
  //***************************************************************************
  
  // "numbers" - Rational, bigint, bigdecimal, whatever. But an actual, known number.
  // Namely, a known number that isn't irrational
  case class NumberValue(num:Int, den:Int) extends Value {
    def + (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2 + num2*den,den*den2))
      case Unbound(sym) => simplify_compound(Compound("+", this, sym))
      case c:Compound => simplify_compound(Compound("+", this, c))
    }
    def - (rhs: Value):Value = simplify(NumberValue(-num, den) + rhs)
    def * (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num2*num, den2*den))
      case otherwise => simplify_compound(Compound("*", this, rhs))
    }
    def / (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => simplify(NumberValue(num*den2, num2*den))
      case otherwise => simplify_compound(Compound("/", this, rhs))
    }
    def ^ (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => {
        if (den2==1) 
          NumberValue(pow(num,num2).toInt, pow(den,den2).toInt)
        else 
          simplify(Compound("^",NumberValue(pow(num,num2).toInt, pow(den,den2).toInt), NumberValue(1,den2)))
      }
      case otherwise => simplify(Compound("^", this, rhs))
    }
    def OVER (rhs: Value):Value = rhs match {
      case NumberValue(num2,den2) => NumberValue(num*den2, den*num2)
      case otherwise => simplify(Compound("/", this, rhs))
    }
  }
  implicit def Int2Value(x:Int) = NumberValue(x,1)
  implicit def Double2Value(x:Double) = {
    var decimals = (x.toString).length - (x.toString).indexOf('.') -1
    decimals = min(decimals, 4) //higher precision might cause overflow in operations :(
    val digits = pow(10,decimals)
    val num = (x*digits).toInt
    val den = digits.toInt
    simplify(NumberValue(num,den))
  }

  
  //unbound variables
  case class Unbound(sym:Symbol) extends Value {
    def + (rhs: Value): Value = simplify(Compound("+",this, rhs))
    def - (rhs: Value): Value = simplify(Compound("-",this, rhs))
    def * (rhs: Value): Value = simplify(Compound("*",this, rhs))
    def / (rhs: Value): Value = simplify(Compound("/",this, rhs))
    def ^ (rhs: Value):Value = simplify(Compound("^", this, rhs))
  }

   
  //expressions with unbound variables 
  case class Compound(op: String, lhs: Value, rhs: Value) extends Value {
    def + (rhs: Value): Value = simplify(Compound("+", this, rhs))
    def - (rhs: Value): Value = simplify(Compound("-", this, rhs))
    def * (rhs: Value): Value = simplify(Compound("*", this, rhs))
    def / (rhs: Value): Value = simplify(Compound("/", this, rhs))
    def ^ (rhs: Value):Value = simplify(Compound("^", this, rhs))
  }
  
  
  //unary minus
  def neg(rhs:Value):Value = Compound("-",0,rhs)

  
  
  //***************************************************************************
  //* INSTRUCTIONS IN OUR LANGUAGE:
  //***************************************************************************
  
  case class Variable(variableName:Symbol) {
    def :=(value:Value) = {
      scope += (variableName -> value)
    }
  }
  
  implicit def symbolToVariable(variableName:Symbol):Variable = Variable(variableName)
  
  val operators : String = "+-*/^" //add more if needed later
  val precedence = Array(4,4,3,3,2) //let's all stick to https://en.wikipedia.org/wiki/Order_of_operations
    // Wikipedia: The source of mathematical truth. :-)
  
  //PRINTLN syntax: PRINTLN(whatever)
  def PRINTLN(value: Value, approximate:Boolean = false): Unit = value match {
    case NumberValue(n,d) => {
      if (!approximate) { if (d==1) println(n) else println(n+"/"+d) }
      else println(n.toDouble/d.toDouble) 
    }
    case Unbound(sym) => println(sym) 
    case Compound(op,lhs,rhs) => {
      PRINT(simplify(value),approximate)
      println
    }
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
  
  def PRINTLN_USE_BINDINGS(value:Value) = PRINTLN_EVALUATE(value)
  
  // PRINTLN_EVALUATES syntax: PRINTLN(whatever).. and evaluates the whatever exactly
  def PRINTLN_EVALUATE(value: Value): Unit = value match {
    case NumberValue(n,d) => println(n+"/"+d)
    case Unbound(sym) => println(sym) 
    case Compound(op,lhs,rhs) => {
      PRINT(getCompoundWithBindings(value.asInstanceOf[Compound]))
      println
    }
  }
  
    // PRINTLN_APPROXIMATE syntax: PRINTLN(whatever).. and evaluates the whatever exactly
  def PRINTLN_APPROXIMATE(value: Value): Unit = value match {
    case NumberValue(n,d) => println(n.toDouble/d.toDouble)
    case Unbound(sym) => if (isknown(sym)) println(approx(sym)) else println(sym) 
    case Compound(op,lhs,rhs) => {
      PRINTLN(getCompoundWithBindings(value.asInstanceOf[Compound],true),true)
    }
  }
  
  
  // PRINTSTRING syntax: PRINTSTRING(myString: String)
  def PRINTSTRING(value : String) : Unit = println(value)
  
  
  
 //*****************************************************************
 //* STUFF TO DEAL WITH VARIABLES:
 //*****************************************************************  
  
  //bindings of variables stored here:
  var scope:Map[Symbol,Value] = Map()
  
  //known values such as pi, e .. can add more
  //if we increase precision, increase precision of these as well.. but not too much or operations with lots of these wil overflow and mess up
  var knownscope:Map[Symbol,Double] = Map(('e,2.7182), ('pi,3.1415))
  
  //look up a variable in our bindings
  implicit def variableLookup(sym:Symbol):Value = scope.get(sym) match {
    case Some(value) => value
    case None => Unbound(sym)
  }
  
  //checks if symbol is known
  def isknown(sym:Symbol): Boolean = {
    knownscope.contains(sym)
  }
    
  //returns approximation of known symbols
  def approx(sym:Symbol): Double = knownscope.get(sym) match {
    case Some(value) => value
    case None => 0.0 //your risk if you call on unknown symbol
  }
  
  

  
  //***************************************************************************
  //* HELPER METHODS.
  //***************************************************************************
  
  def simplify_compound(v:Value, approximate:Boolean = false):Value = simplify(v,approximate)

  def simplify(v:Value, approximate:Boolean = false):Value = v match {
    case NumberValue(n,d) => {
      val g = gcd(n,d)
      val l = lcm(n,d)
      //println(s"gcd($n,$d) = $g")
      //println(s"lcm($n,$d) = $l")
      NumberValue(n / g, d / g)
    }
    case c:Compound => c.op match {
      case "+" => {
        // See whether lhs can be distributed across rhs (or vice-versa)
        c.lhs match {
          case lhs_nv:NumberValue => c.rhs match {
            // TODO: What if both lhs2 and rhs2 are NumberValues?
            case Compound("+",lhs2:NumberValue,rhs2) => Compound("+", lhs2 + lhs_nv, rhs2)
            case Compound("+",lhs2,rhs2:NumberValue) => Compound("+", lhs2, rhs2 + lhs_nv)
            case rhs_nv:NumberValue => lhs_nv + rhs_nv
            case otherwise => c
          }
          case otherwise => c
        }
      }
      case "*" => {
        // See whether lhs and rhs are both NVs
        // Also, if one is an addition/subtraction then we can/should distribute
        c.lhs match {
          case lhs_nv:NumberValue => c.rhs match {
            case rhs_nv:NumberValue => lhs_nv * rhs_nv
            case otherwise => c
          }
          case otherwise => c
        }
      }
      case "/" => {
        c.lhs match {
          case lhs_nv:NumberValue => c.rhs match {
            case rhs_nv:NumberValue => lhs_nv / rhs_nv
            case otherwise => c
          }
          case otherwise => c
        }
      }
      case "^" => {
        if (approximate) {
          c.lhs match {
          case lhs_nv:NumberValue => c.rhs match {
            case rhs_nv:NumberValue => pow(lhs_nv.num.toDouble/lhs_nv.den.toDouble, rhs_nv.num.toDouble/rhs_nv.den.toDouble)
            case otherwise => c
          }
          case otherwise => c
        }
        }
        else c
      }
      case otherwise => c
    }
    case otherwise => v
  }
  
  
  
  //*******************************
  //* Runs the test() method when
  //* used in the DSL
  //*******************************
  def TEST() : Unit = test()
  
  /**
   * Arbitrary test method.
   */
  def test() {
    //variableLookup
    var compound1 = Compound("+", NumberValue(1,1), NumberValue(3,1))
    var compound2 = Compound("-", compound1, NumberValue(45,1))
    var compound3 = Compound("*", compound1, compound2)
    if (allNumberValues(compound3)) {
      PRINTLN(compound3)
      PRINTSTRING("SUCCESS")
    }
    else {
      PRINTLN(compound3)
      PRINTSTRING("FAIL")
    }
  }
   

  /**
   * Given a Compound, return a new Compound in which all unbound
   * variables are replaced by their bindings, if such a binding
   * exists.
   */
  def getCompoundWithBindings(compound: Compound, approximate: Boolean = false): Value = {
    
    // The final new lhs and rhs for this compound. These are
    // built recursively.
    var newLhs: Value = null
    var newRhs: Value = null
    var op: String = compound.op
    
    // If the lhs is a compound, recursively call it.
    if (isCompound(compound.lhs)) {
      newLhs = getCompoundWithBindings(compound.lhs.asInstanceOf[Compound], approximate)
    }
    // Else if lhs is a number value, don't change it.
    else if (isNumberValue(compound.lhs)) {
      newLhs = compound.lhs;
    }
    // Else it is a variable, and we can try to replace it with a binding. If
    // it has no binding, it will not change.
    else {
      newLhs = variableLookup(compound.lhs.asInstanceOf[Unbound].sym)
      if (approximate && isknown(compound.lhs.asInstanceOf[Unbound].sym))
        newLhs = approx(compound.lhs.asInstanceOf[Unbound].sym)
    }
    
    // If the rhs is a compound, recursively call it.
    if (isCompound(compound.rhs)) {
      newRhs = getCompoundWithBindings(compound.rhs.asInstanceOf[Compound],approximate)
    }
    // Else if rhs is a number value, don't change it.
    else if (isNumberValue(compound.rhs)) {
      newRhs = compound.rhs
    }
    // Else it is a variable, and we can try to replace it with a binding. If
    // it has no binding, it will not change.
    else {
      newRhs = variableLookup(compound.rhs.asInstanceOf[Unbound].sym)
      if (approximate && isknown(compound.rhs.asInstanceOf[Unbound].sym)) 
        newRhs = approx(compound.rhs.asInstanceOf[Unbound].sym)
    }
    
    return simplify_compound(Compound(op, newLhs, newRhs),approximate);
  }

  // Returns the LCM of a and b
  def lcm(a:Int, b:Int):Int = a*b / gcd(a,b);

  //Returns the greatest common denominator of a and b
  def gcd(a:Int, b:Int):Int = {
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
      /*val q = a / b;
      val r = a - q*b;
      println(s"gcd($a,$b) => $q,$r")
      if (r == 0) {
        return -1;
      }
      val g = gcd(b, r);
      if (g == -1) {
        return r;
      }
      return g;*/
    }
  }
  
  // Returns true iff the given compound is made purely of
  // IntValues or DoubleValues (i.e. can be simplified to a number without a variable binding)
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
  def getNum(value: Value): Int = value match {
    case NumberValue(n,d) => n
    case otherwise => 0 //your risk to call this on st that is not numbervalue
  }
  
  //gets numerator out of fraction
  def getDen(value: Value): Int = value match {
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
  
  //gets symbol out of unbound
  def getSym(value: Value): Symbol = value match {
    case Unbound(s) => s
    case otherwise => 'youwrong //your risk to call this on st that is not unbound
  }
  
}

