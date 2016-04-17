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
  implicit def Int2Value(x:Int):NumberValue = NumberValue(x,1)
  implicit def Double2Value(x:Double):Value = {
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
  
  /*
   * Used an as intermediate data structure for simplifying compounds. This should
   * not shine through to the user--they should not know of this data structure.
   */
  case class CompoundCluster(ops: List[String], children: List[Value]) extends Value {
    
    override def toString: String = return flattenCompoundClusterToString(this);
    
    // These are useless and should not be used for now.
    def + (rhs: Value): Value = null;
    def - (rhs: Value): Value = null;
    def * (rhs: Value): Value = null;
    def / (rhs: Value): Value = null;
    def ^ (rhs: Value): Value = null;
    def OVER (rhs: Value): Value = null;
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
  val precedenceMap = Map("+" -> precedence(0), "-" -> precedence(1), "*" -> precedence(2), "/" -> precedence(3), "^" -> precedence(4));
  
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
  
  def GET_PRINT_STRING(value: Value, approximate:Boolean = false): String = value match {
    case NumberValue(n,d) => {
      if (!approximate) { if (d==1) return n.toString() else return (n+"/"+d).toString(); }
      else return (n.toDouble/d.toDouble).toString();
    }
    case Unbound(sym) => return (sym.toString).substring(1); //get rid of '
    
    // This case doesn't matter for the use of the simplify compound method by Mike.
    /*case Compound(op,lhs,rhs) => {
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
    }*/
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
  
  def simplify(v:Value, approximate:Boolean = false):Value = v match {
    case NumberValue(n,d) => {
      val g = gcd(n,d)
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
  
  
  
  //*****************************************************
  //* Runs the test() method when TEST is used in the DSL
  //*****************************************************
  def TEST() : Unit = test()
  
  /**
   * Arbitrary test method.
   */
  def test() {
    
    var compound1 = Compound("+", Unbound('a), NumberValue(3,1));
    var compound2 = Compound("-", NumberValue(4,1), Unbound('a));
    var compound3 = Compound("-", compound1, compound2);
    var compound4 = Compound("+", NumberValue(55,1), compound1);
    var compound5 = Compound("-", compound4, compound2);
    var compound6 = Compound("+", compound1, compound2);
    
    var longCompoundTest = Compound("+", Compound("-", Unbound('a), Unbound('a)), Compound("-", Unbound('a), Unbound('a)));
    
    //var compound4 = Compound("+", Unbound('x), compound)
    //var compound4 = Compound("-", compound3, NumberValue(5,1));
    //var compound5 = Compound("+", NumberValue(1,1), NumberValue(1,1));
    
    // This is 'x := 'a + 'b + 'c
    var test = Compound("+",Compound("+",Unbound('a),Unbound('b)),Unbound('c));
    
    // This is 'x := 'a + 'b + 'c + 'd
    var test2 = Compound("+",Compound("+",Compound("+",Unbound('a),Unbound('b)),Unbound('c)),Unbound('d));

    var test3 = Compound("-", test, test2);
    
    var test4 = Compound("*", compound3, compound5);
    
    var test5 = Compound("*", compound6, compound5);
    //var newCompound = simplifyCompoundNumberValuePairs(compound4);
    //PRINTLN(compound3);
    //PRINTLN(newCompound);
    
    //println("Testing for flatten to String: ");
    
    //println(flattenCompoundToString(test2));
    //var cc1: CompoundCluster = compoundToCompoundCluster(test2);
    //println(flattenCompoundClusterToString(cc1));
    //println(flattenCompoundToString(test3));
    
    //var cc2: CompoundCluster = compoundToCompoundCluster(test3);
    //println(flattenCompoundClusterToString(cc2));
    
    println("Testing CompoundCluster stuff: " );
    
    var cc3: CompoundCluster = compoundToCompoundCluster(compound3);
    // ((a + 3) - (4 - a))
    println(flattenCompoundClusterToString(cc3));
    
    var cc4: CompoundCluster = compoundToCompoundCluster(compound5);
    // ((55 + (a + 3)) - (4 - a))
    println(flattenCompoundClusterToString(cc4));
    
    var cc5: CompoundCluster = compoundToCompoundCluster(longCompoundTest);
    // ((a - a) + (a - a))
    println(flattenCompoundClusterToString(cc5));
    
    var cc6: CompoundCluster = compoundToCompoundCluster(test4);
    // (((a + 3) - (4 - a)) * ((55 + (a + 3)) - (4 - a)))
    println(flattenCompoundClusterToString(cc6));
    
    var cc7: CompoundCluster = compoundToCompoundCluster(test5);
    // (((a + 3) + (4 - a)) * ((55 + (a + 3)) - (4 - a)))
    println(flattenCompoundClusterToString(cc7));

    
    //println(flattenCompoundToString(test));
    //println(flattenCompoundToString(test2));
    //println(flattenCompoundToString(test3));
    //println(flattenCompoundToString(compound3));
    //PRINT(compound1);
    
    //********************************
    //* Testing merge values function
    //********************************
    
    println("\nTESTING MERGE GROUPS FUNCTION: ");
    
    // (a + 3 - 4 + a)
    var group1: CompoundCluster = mergeGroups(cc3.ops(0), cc3.children(0).asInstanceOf[CompoundCluster], cc3.children(1).asInstanceOf[CompoundCluster]);
    println(group1);
    
    // (a + 3 + 4 - a)
    var group2: CompoundCluster = mergeGroups("+", cc3.children(0).asInstanceOf[CompoundCluster], cc3.children(1).asInstanceOf[CompoundCluster]);
    println(group2);
    
    // ((a + 3) * (4 - a))
    var group3: CompoundCluster = mergeGroups("*", cc3.children(0).asInstanceOf[CompoundCluster], cc3.children(1).asInstanceOf[CompoundCluster]);
    println(group3);
    
    // ((a + 3) / (4 - a))
    var group4: CompoundCluster = mergeGroups("/", cc3.children(0).asInstanceOf[CompoundCluster], cc3.children(1).asInstanceOf[CompoundCluster]);
    println(group4);
    
    // (55 + a + 3 - 4 + a)
    var group5: CompoundCluster = mergeGroups(cc4.ops(0), cc4.children(0).asInstanceOf[CompoundCluster], cc4.children(1).asInstanceOf[CompoundCluster]);
    println(group5);
    
    // (a - a + a - a)
    var group6: CompoundCluster = mergeGroups(cc5.ops(0), cc5.children(0).asInstanceOf[CompoundCluster], cc5.children(1).asInstanceOf[CompoundCluster]);
    println(group6);
    
    // ((a + 3 - 4 + a) * (55 + a + 3 - 4 + a))
    var group7: CompoundCluster = mergeGroups(cc6.ops(0), cc6.children(0).asInstanceOf[CompoundCluster], cc6.children(1).asInstanceOf[CompoundCluster]);
    println(group7);
    
    // ((a + 3 + 4 - a) * (55 + a + 3 - 4 + a))
    var group8: CompoundCluster = mergeGroups(cc7.ops(0), cc7.children(0).asInstanceOf[CompoundCluster], cc7.children(1).asInstanceOf[CompoundCluster]);
    println(group8);
    
    println("\nFINAL SIMPLIFY TESTS BELOW: ");
    
    // Should be: (a + -1 + a)
    println(simplifyGroups(group1));
    
    // Should be: 7
    println(simplifyGroups(group2));
    
    // Should be: (54 + a + a)
    println(simplifyGroups(group5));
    
    // Should be: 0
    println(simplifyGroups(group6));
    
    // Should be: ((a + -1 + a) * (54 + a + a))
    println(simplifyGroups(group7));
    
    // Should be: 
    println(simplifyGroups(group8));
    
    
  }
  
  
  /**
   * Given a CompoundCluster tree structure where each node is of the form
   * (lhs <op> rhs) (i.e., only two children per node), return an equivalent
   * CompoundCluster tree structure in which leaf nodes are groups, where a
   * group is defined as containing the maximum number of children based on 
   * rules of precedence.
   * 
   * In other words, the leaf nodes of this CompoundCluster will be groups, 
   * and groups will gather all NumberValues and Unbounds in parts of 
   * the expression that can be possibly combined based on the rules of
   * precedence. These groups can then be simplified, where cancellation and
   * computation is possible.
   * 
   * Note that groups will only be leaf nodes, and all other nodes in the
   * returned tree structure will be of the same form (lhs <op> rhs).
   * A semi-formal explanation for this is given in the method body.
   */
  def mergeGroups(op: String, lhs: Value, rhs: Value): CompoundCluster = {
    
    // For two given leaf CompoundCluster objects, lhs, and rhs, get the 
    // precedence of the lhs, the rhs, and the operator connecting
    // them. If they are all the same, then we can combine the lhs and the rhs 
    // with the operator connecting them into one new leaf CompoundCluster. 
    // Else, we cannot connect them, so do nothing.
    
    // Note that CompoundClusters are created from Compounds directly, thus
    // they all start having form (op, lhs, rhs), where lhs and rhs are a
    // Value (i.e., either a CompoundCluster or a terminal value such as 
    // Unbound or NumberValue). Thus, each CompoundCluster starts off in the form
    // (lhs <op> rhs). By construction, each CompoundCluster has only operators
    // of the same precedence because there exists only one operator initially
    // (which equals itself by definition), and the way that CompoundClusters
    // are simplified only allows for two CompoundClusters to come together
    // when all the operators on the lhs and rhs are of the same precedence,
    // also matching their connecting operator. Since the only way to add more
    // operators to a CompoundCluster lhs or rhs is by the simplification outlined 
    // above, it follows that all of the operators in any lhs and rhs are always 
    // equal precedence. Thus, we must only check the first operator on the lhs 
    // the first on the rhs to find what the overall precedence level of the lhs 
    // or rhs is (i.e., the precedence of an entire group is the same as of its
    // first operator).
    
    // Furthermore, we are safe assuming that the lhs and rhs have at least one
    // operator, by construction as detailed above. It is then safe to always
    // access the first element (index 0) of the operator list of a given
    // CompoundCluster.
    
    // Case 1: (CompoundCluster <op> CompoundCluster)
    // Case 2: (CompoundCluster <op> non-CompoundCluster)
    // Case 3: (non-CompoundCluster <op> CompoundCluster)
    // Case 4: (non-CompoundCluster <op> non-CompoundCluster)
    
    // In Case 1, we must only check the first operator in each CompoundCluster
    // (i.e., in both lhs and rhs).
    
    // In Case 2, there is no operator to check on the lhs, thus we must check
    // only the precedence of <op> and the lhs.
    
    // In Case 3, there is no operator to check on the rhs, thus we must check
    // only the precedence of <op> and the rhs.
    
    // In Case 4, there is no operator to check on the rhs or the lhs, thus
    // we can create a CompoundCluster directly without checking.
    
    // Case 1:
    if (isCompoundClusterValue(lhs) && isCompoundClusterValue(rhs)) {
      
      // Try to merge the lhs recursively.
      var lhsCC = lhs.asInstanceOf[CompoundCluster];
      var newLhs = mergeGroups(lhsCC.ops(0), lhsCC.children(0), lhsCC.children(1));
      
      // Try to merge rhs recursively
      var rhsCC = rhs.asInstanceOf[CompoundCluster];
      var newRhs = mergeGroups(rhsCC.ops(0), rhsCC.children(0), rhsCC.children(1));
      
      // Get precedence levels of the lhs group, the rhs group, and the 
      // operator trying to connect them.
      var lhsPrecedenceLevel: Int = precedenceMap(newLhs.ops(0));
      var opPrecedenceLevel: Int = precedenceMap(op);
      var rhsPrecedenceLevel: Int = precedenceMap(newRhs.ops(0));
      
      // We can merge the groups iff all precedence levels are equal.
      if ((lhsPrecedenceLevel == opPrecedenceLevel) && (opPrecedenceLevel == rhsPrecedenceLevel)) {
        
        // If in the format: (lhs - rhs), and the rhs is made up of only
        // + and - operators (as it is here), we will convert each +
        // in the rhs operator list to a -, and convert each + to a -.
        var alternateNewRhsOps: List[String] = List();
        
        if (op.equals("-")) {
          
          for (element <- newRhs.ops) {  
            var newElement = element match {
              case "+" => "-";
              case "-" => "+";
            }
            
            alternateNewRhsOps = alternateNewRhsOps.:+(newElement);
          }
          
        }
        else {
          // Common case.
          alternateNewRhsOps = newRhs.ops;
        }
        
        //println("newRHS.ops: " + newRhs.ops);
        //println("alternateNewRhsOps: " + alternateNewRhsOps);
        
        // For ops list we want: (lhs.ops, op, rhs.ops).
        var newOps: List[String] = newLhs.ops :+ op;
        newOps = newOps ::: alternateNewRhsOps;
        
        // For children list we want: (lhs.children, rhs.children).
        var newChildren: List[Value] = newLhs.children ::: newRhs.children;
        
        return new CompoundCluster(newOps, newChildren);
      }
      // Else, we could not merge them into one, so return the original
      // format of (lhs <op> rhs), but with lhs and rhs possibly
      // optimized (i.e., subparts were merged recursively).
      else {
        return new CompoundCluster(List(op), List(newLhs, newRhs));
      }
    }
    
    // Case 2:
    if (isCompoundClusterValue(lhs) && !isCompoundClusterValue(rhs)) {
      
      // Try to merge the lhs recursively.
      var lhsCC = lhs.asInstanceOf[CompoundCluster];
      var newLhs = mergeGroups(lhsCC.ops(0), lhsCC.children(0), lhsCC.children(1));
      
      var lhsPrecedenceLevel: Int = precedenceMap(newLhs.ops(0));
      var opPrecedenceLevel: Int = precedenceMap(op);
      
      if (lhsPrecedenceLevel == opPrecedenceLevel) {
        
        // For ops list we want: (lhs.ops, op).
        var newOps: List[String] = newLhs.ops :+ op;
        
        // For children list we want: (lhs.children, rhs).
        var newChildren: List[Value] = newLhs.children :+ rhs;
        
        return new CompoundCluster(newOps, newChildren);
      }
      // Else, we could not merge them into one, so return the original
      // format of (lhs <op> rhs), but with lhs and rhs possibly
      // optimized (i.e., subparts were merged recursively).
      else {
        return new CompoundCluster(List(op), List(newLhs, rhs));
      }
    }
    
    // Case 3:
    if (!isCompoundClusterValue(lhs) && isCompoundClusterValue(rhs)) {
      
      // Try to merge rhs recursively
      var rhsCC = rhs.asInstanceOf[CompoundCluster];
      var newRhs = mergeGroups(rhsCC.ops(0), rhsCC.children(0), rhsCC.children(1));
      
      var rhsPrecedenceLevel: Int = precedenceMap(newRhs.ops(0));
      var opPrecedenceLevel: Int = precedenceMap(op);
      
      if (rhsPrecedenceLevel == opPrecedenceLevel) {
        
        // If in the format: (lhs - rhs), and the rhs is made up of only
        // + and - operators (as it is here), we will convert each +
        // in the rhs operator list to a -, and convert each + to a -.
        var alternateNewRhsOps: List[String] = List();
        
        if (op.equals("-")) {
          
          for (element <- newRhs.ops) {  
            var newElement = element match {
              case "+" => "-";
              case "-" => "+";
            }
            
            alternateNewRhsOps = alternateNewRhsOps.:+(newElement);
          }
          
        }
        else {
          // Common case.
          alternateNewRhsOps = newRhs.ops;
        }
        
        // For ops list we want: (op, rhs.ops).
        var newOps: List[String] = alternateNewRhsOps.+:(op);
        
        // For children list we want: (lhs, rhs.children).
        var newChildren: List[Value] = newRhs.children.+:(lhs);
        
        return new CompoundCluster(newOps, newChildren);
      }
      // Else, we could not merge them into one, so return the original
      // format of (lhs <op> rhs), but with lhs and rhs possibly
      // optimized (i.e., subparts were merged recursively).
      else {
        return new CompoundCluster(List(op), List(lhs, newRhs));
      }
    }
    
    // Case 4:
    if (!isCompoundClusterValue(lhs) && !isCompoundClusterValue(rhs)) {
      
      // Merge the terminal values.
      return new CompoundCluster(List(op), List(lhs, rhs));
    }
    
    // Should never get here.
    return null;
  }
  
  
  /**
   * Given a CompoundCluster tree structure of groups, return an equivalent,
   * simplified CompoundCluster of groups in which each group is simplified.
   * Simplification is defined as combining as many NumberValues within groups
   * as possible, and removing variables of the form {-v, v}, where v is an
   * Unbound.
   * 
   * Note that this algorithm expects a tree structure in which only leaf
   * CompoundClusters are groups, which can be recursively simplified, thus
   * shortening the height of the tree. This implies that each node has only
   * two branches, namely of the form (lhs, op, rhs) where lhs and rhs are
   * groups. When a group is simplified, it is only possible to create a new
   * node of this form.
   */
  def simplifyGroups(compoundCluster: CompoundCluster): Value = {
    
    // The new children and new operator lists of this CompoundCluster.
    // At the end of this method, a new CompoundCluster is returned using
    // these values, which are modified throughout the method.
    var newChildren: List[Value] = compoundCluster.children;
    var newOps: List[String] = compoundCluster.ops;
    
    // We must simplify from the leaves up, since we can only simplify groups,
    // and groups are leaf nodes. Only simplify if none of the children of a 
    // given node are CompoundClusters (i.e., only simplify leaf nodes).
    
    // If any children are CompoundCluster type, simplify them recursively before
    // we analyze this group.
    var elementIndex: Int = 0;
    for (element: Value <- compoundCluster.children) {
    
      if (isCompoundClusterValue(element)) {
        
        // Recursively simplify this child's groups.
        var simplifiedChild: Value = simplifyGroups(element.asInstanceOf[CompoundCluster]);
        newChildren = newChildren.updated(elementIndex, simplifiedChild);
      }
      
      elementIndex += 1;
    }
    
    //***********************************
    //* Simpilify the given group below.
    //***********************************
    
    //println("CURRENT GROUP TO SIMPLIFY: " + new CompoundCluster(newOps, newChildren));
    
    // It is possible that 
    
    // TODO ??
    // Try to merge groups, because the LHS and RHS may have turned into
    // something which can be turned into a single group, and thus needs
    // to be grouped before this method is performed. This is only possible
    // to do if there is a single LHS and RHS and operation.
    //if ((newChildren.length == 2) && (newOps.length == 1)) {
      //println("Children passing in: " + newChildren(0));
      //println("Children passing in: " + newChildren(1));
      //println("Ops passing in: " + newOps);
      //var newClusterCompound = mergeGroups(newOps(0), newChildren(0), newChildren(1));
      //newChildren = newClusterCompound.children;
      //newOps = newClusterCompound.ops;
    //}
    
    
    //******************************************************************
    //* Try to find matching Unbound objects of the couple {a, -a}.
    //* If we find one, cancel them out. For now, we do not do anything
    //* in terms of making something of the form {a, a} = {2a}.
    //******************************************************************
    var stillSimplyfingUnbounds: Boolean = true;
    
    while (stillSimplyfingUnbounds) {
      
      //println("NEW CHILDREN: " + newChildren);
      //println("NEW OPS: " + newOps);
      
      var varName: Symbol = null;
      var varFound1: Boolean = false;
      var varFound2: Boolean = false;
      var positive1: Boolean = false;
      var position1: Int = -1;
      var positive2: Boolean = false;
      var position2: Int = -1;
      var currElementIndex : Int = 0;
    
      // Traverse through children and try to find matching elements children
      // of the form {v, -v} where a is an Unbound.
      for (element: Value <- newChildren) {
      
        // If this element is an Unbound, check it out.
        if (isUnbound(element)) {
        
          // If we are still looking for our first Unbound (v).
          if (!varFound1) {
          
            // If we haven't found one yet, use this one.
            // Note it's symbol and position in the children list.
            varFound1 = true;
            varName = element.asInstanceOf[Unbound].sym;
            position1 = currElementIndex;
          
            // Find whether this Unbound is positive or negative.
            if (position1 == 0) {
              // If there are no operators before it, then it is a positive
              // unbound.
              positive1 = true;
            }
            else {
              // If there are operators before it, then it is a positive unbound
              // iff the operator is not a minus sign.
              positive1 = !(newOps(currElementIndex - 1) == "-");
            }
          }
          
          // If we have found the first Unbound (v), look for a second 
          // Unbound of the form (-v).
          else {
            
            // Only look if we haven't already found one of the form (-v).
            if (!varFound2) {
            
              // We have found an element of the symbol (v). Check to see if
              // it is (-v) by looking at its sign.
              if (element.asInstanceOf[Unbound].sym.equals(varName)) {
              
                // Note it's position in the children list.
                position2 = currElementIndex;
              
                // Check if it is the opposite sign. If so, we found our 
                // second var and we can stop looking, we will cancel them 
                // out after the loop is done iterating. It cannot be the 
                // first child by definition, since this is the second 
                // variable, so it is safe to assume it has a sign before it.
                positive2 = !(newOps(currElementIndex - 1) == "-");
              
                // If the first and the second variable are opposite signs,
                // we can cancel them.
                if (positive1 != positive2)
                {
                  varFound2 = true;
                  
                  // We should break here, but it's complex and ugly in Scala, so
                  // the loop will iterate fully, but no longer look for any
                  // matching variables; we've found our two for this cycle of
                  // iterations.
                }
                
                // If not, then we will keep looking for a second.
              }
            }
            else {
            
              // We have already found the first and second, don't care, keep
              // iterating.
            }
          }
        
        }
      
        currElementIndex += 1;
      }
    
      //****************************************************
      //* If we found matching variables, cancel them out.
      //****************************************************
      if (varFound1 && varFound2) {
        
        stillSimplyfingUnbounds = true;
        
        //println("FOUND SOME TO DELETE: " + varName);
        //println("position1: " + position1);
        //println("position2: " + position2);
    
        // Remove the first Unbound from the group.
        newChildren = newChildren.take(position1) ++ newChildren.drop(position1 + 1);
    
        // Remove the second Unbound from the group. Note: we know that the second
        // position is greater than the first position, so we are okay subtracting
        // one from the second position--there is no chance of out of bounds.
        newChildren = newChildren.take(position2 - 1) ++ newChildren.drop(position2);
    
        //println("NEW CHILDREN: " + newChildren);
    
        // Remove the first operator from the group. We want to remove the operator
        // to the direct left of the Unbound we removed. If we removed the first
        // Unbound in a group, then we must remove the same index operator.
        if (position1 == 0) {
      
          newOps = newOps.take(position1) ++ newOps.drop(position1 + 1);
          newOps = newOps.take(position2 - 2) ++ newOps.drop(position2 - 1);
        }
        // Else, we want to remove the operator at the index before whatever the
        // Unbound was at.
        else {
      
          newOps = newOps.take(position1 - 1) ++ newOps.drop(position1);
          newOps = newOps.take(position2 - 2) ++ newOps.drop(position2 - 1);
        }
    
        // If we removed the first two children, there will be an extra 
        // operator to the far left, since we have wiped out the first 
        // two Values and we only remove the operators to the left
        // of removed Values in the general case.
        if ((position1 == 0) && (position2 == 1)) {
        
          newOps = newOps.drop(1);
        }
    
          //println("NEW OPS: " + newOps);
      }
      else {
        
        stillSimplyfingUnbounds = false;
      }
    } // Check if there exists another pair of matching variables (while loop).
    
    
    // Special condition: if the expression was purely Unbounds and they 
    // all cancelled, make the result 0.
    if (newChildren.length == 0) {
      
      newChildren = List(NumberValue(0,1));
    }
    
    //*************************************************************************
    //* At this point, all Unbounds are simplified for the given group. Now, 
    //* simplify NumberValues in the given group.
    //*************************************************************************
    
    var stillSimplifyingNumbers: Boolean = true;
    
    while (stillSimplifyingNumbers) {
    
      // Simplify numbers. We are able to traverse the group one time, keeping
      // a running sum of the NumberValues. We will leave the Unbounds in their
      // place if we come across them, since they have already been taken care
      // of.
      var currLHS: NumberValue = null;
      var currRHS: NumberValue = null;
      var currIndex: Int = 0;
      var lhsIndex: Int = -1;
      var rhsIndex: Int = -1;
      var currOp: String = null;
      var foundLHS: Boolean = false;
      var foundRHS: Boolean = false;
    
      for (element: Value <- newChildren) {
      
        // If not a NumberValue, skip this element.
        if (isNumberValue(element)) {
        
          // If we have found a LHS already.
          if (foundLHS) {
            
            // If we have not found a RHS, but have found a LHS.
            if (!foundRHS) {
          
              foundRHS = true;
          
              // Store information about it to use later.
              
              // We have found a RHS. We will take the LHS and the RHS,
              // do the appropriate operation on them, and store the result
              // in the place of the LHS. Thus, a final NumberValue result
              // will remain in the LHS index at the end of the computation
              // of joining NumberValues.
              currRHS = element.asInstanceOf[NumberValue];
              rhsIndex = currIndex;
          
              // Note that the RHS cannot be the first child, because if the first
              // child were a NumberValue, it would have been taken by the LHS.
              // Thus, the operator before the RHS is always at the same index
              // relative to the index of the RHS.
              currOp = newOps(currIndex - 1);
              
              // The loop will keep iterating, because we are not using break,
              // but we have all the information we need about the LHS and the
              // RHS. We can keep iterating without worry, it is impossible for
              // the LHS and RHS to be overwritten--the loop will just spin
              // until done.
            }
          }
          
          // If we have not found a LHS, take this one.
          else {
            
            // Store information about it to use later.
            currLHS = element.asInstanceOf[NumberValue];
            lhsIndex = currIndex;
            foundLHS = true;
        
            // We now have all necessary information for the LHS. Find a RHS.
            
            }
        }
      
        currIndex += 1;
      }
    
      // Once we've found a LHS and RHS, do the computation and remove
      // the proper elements from the children and ops list.
      if (foundLHS && foundRHS) {
        
        // Remove the RHS and its operator from their lists respectively.
        // Because of the nature of the RHS, the indices to remove are
        // straight forward.
        newOps = newOps.take(rhsIndex - 1) ++ newOps.drop(rhsIndex);
        newChildren = newChildren.take(rhsIndex) ++ newChildren.drop(rhsIndex + 1);
          
        //println("NEW OPS:      " + newOps);
        //println("New CHILDREN: " + newChildren);
          
        // Perform the operation.
        var newValue: Value = currOp match {
          
          case "+" => currLHS.+(currRHS);
          case "-" => currLHS.-(currRHS);
          case "*" => currLHS.*(currRHS);
          case "/" => currLHS./(currRHS);
        }
          
        var newNumberValue: NumberValue = newValue.asInstanceOf[NumberValue];
          
        //println("NEW NUMBER VALUE: " + newNumberValue);
          
        // Replace the LHS with the result.
        newChildren = newChildren.updated(lhsIndex, newNumberValue);
      }
      
      // We are done simplifying NumberValues.
      else {
        stillSimplifyingNumbers = false;
      }
    }
    
    
    // If a group is simplified then it will not be able to be
    // grouped with its parent operator (i.e., a group of + and - cannot
    // group with a parent operator of * or /) unless the leaf group
    // evaluates to a single Unbound or NumberValue. It must be rid of all
    // the operators. This is possible, however, and we need to take this into
    // account. If the result is a single Unbound or NumberValue, return it
    // in this format.
    if (newChildren.length == 1) {
      
      if (isUnbound(newChildren(0))) {
        
        return newChildren(0);
      }
      else if (isNumberValue(newChildren(0))) {
        
        return newChildren(0);
      }
    }
    
    // Return the new CompoundCluster.
    return new CompoundCluster(newOps, newChildren);
  }
  
  
  /**
   * Converts a Compound to a CompoundCluster.
   */
  def compoundToCompoundCluster(compound: Compound): CompoundCluster = {
    
    // Base case: neither the LHS or RHS are compounds. Thus, this
    // is a leaf compound. Convert it to a CompoundCluster.
    if (!isCompound(compound.lhs) && !isCompound(compound.rhs)) {
      return new CompoundCluster(List(compound.op), List(compound.lhs, compound.rhs));
    }
    
    // Either LHS or RHS (or both) are Compounds. Create CompoundClusters
    // with them.
    var lhsIsCompoundCluster: Boolean = false;
    var rhsIsCompoundCluster: Boolean = false;
    var lhsCompoundCluster: CompoundCluster = null;
    var rhsCompoundCluster: CompoundCluster = null;
    
    // If LHS is Compound, convert it to a CompoundCluster.
    if (isCompound(compound.lhs)) {
      lhsCompoundCluster = compoundToCompoundCluster(compound.lhs.asInstanceOf[Compound]);
      lhsIsCompoundCluster = true;
    }
    
    // If RHS is Compound, convert it to a CompoundCluster.
    if (isCompound(compound.rhs)) {
      rhsCompoundCluster = compoundToCompoundCluster(compound.rhs.asInstanceOf[Compound]);
      rhsIsCompoundCluster = true;
    }
    
    // If both RHS and LHS are now CompoundClusters, return a CompoundCluster.
    // composed of them.
    if (lhsIsCompoundCluster && rhsIsCompoundCluster) {
      return new CompoundCluster(List(compound.op), List(lhsCompoundCluster, rhsCompoundCluster));
    }
    
    // Either just RHS is a CompoundCluster, or just LHS is a CompoundCluster
    // at this point.
    
    if (lhsIsCompoundCluster) {
      return new CompoundCluster(List(compound.op), List(lhsCompoundCluster, compound.rhs));
    }
    
    if (rhsIsCompoundCluster) {
      return new CompoundCluster(List(compound.op), List(compound.lhs, rhsCompoundCluster));
    }
    
    // Should never be here.
    return null;
  }
  
  
  /**
   * Flatten the given CompoundCluster to a fully-parenthesized String.
   */
  def flattenCompoundClusterToString(compoundCluster: CompoundCluster): String = {
    
    // Base case: none of the children are CompoundClusters. Thus, this
    // is a leaf CompoundCluster.
    var allChidrenNotCompoundCluster: Boolean = false;
    
    for (element: Value <- compoundCluster.children) {
      if (isCompoundClusterValue(element)) {
        allChidrenNotCompoundCluster = false;
      }
    }
    
    // This is the base case. Return a String representing this leaf 
    // CompoundCluster.
    if (allChidrenNotCompoundCluster) {
      
      var returnString: String = "";
      
      returnString += "(";
      
      var currentOpIndex: Int = 0;
      
      for (element: Value <- compoundCluster.children) {
        
        returnString += GET_PRINT_STRING(element);
        
        // If we still have operators to print.
        if (currentOpIndex < compoundCluster.ops.length) {
          returnString += " " + compoundCluster.ops(currentOpIndex) + " ";
        }
        
        // Move on to the next operator.
        currentOpIndex = currentOpIndex + 1;
      }
      
      returnString += ")";
      
    }
    
    // Else, at least one of the children is a CompoundCluster which needs to
    // be simplified recursively by this method.
    var returnString = "(";
    var currentOpIndex: Int = 0;
    
    // For each child, if it is a Compound Cluster, recursively flatten it to
    // a String, else just use the child's value.
    for (element: Value <- compoundCluster.children) {
      
      // If this child is a CompoundCluster.
      if (isCompoundClusterValue(element)) {
        returnString += flattenCompoundClusterToString(element.asInstanceOf[CompoundCluster]);
      }
      // Else it's not, just take its Value.
      else {
        returnString += GET_PRINT_STRING(element);
      }
      
      // If we still have operators to print.
      if (currentOpIndex < compoundCluster.ops.length) {
          returnString += " " + compoundCluster.ops(currentOpIndex) + " ";
      }
      
      // Move on to the next operator.
      currentOpIndex = currentOpIndex + 1;
    }
    
    returnString += ")";
    
    return returnString;
  }
  
  
  /**
   * Flatten the given Compound to a fully-parenthesized String.
   */
  def flattenCompoundToString(compound: Compound): String = {
    
    // Base case: neither the LHS or RHS are compounds. Thus, this
    // is a leaf compound.
    if (!isCompound(compound.lhs) && !isCompound(compound.rhs)) {
      return "(" + GET_PRINT_STRING(compound.lhs) + " " + compound.op + " " + GET_PRINT_STRING(compound.rhs) + ")";
    }
    
    // Either the LHS or the RHS (or both) are Compounds. Get their strings
    // recursively. Note that compounds are left-associative.
    var lhsString: String = "";
    var rhsString: String = "";
    
    // If LHS is compound, get its string.
    if (isCompound(compound.lhs)) {
      lhsString = flattenCompoundToString(compound.lhs.asInstanceOf[Compound]);
    }
    // Else, it's not a compound, get it's string with its toString method.
    else {
      // Note: for now we are using the same logic as the PRINT method to get
      // String representations for non-Compounds.
      lhsString = GET_PRINT_STRING(compound.lhs);
    }
    
    // If RHS is compound, get its string.
    if (isCompound(compound.rhs)) {
      rhsString = flattenCompoundToString(compound.rhs.asInstanceOf[Compound]);
    }
    // Else, it's not a compound, get it's string with its toString method.
    else {
      // Note: for now we are using the same logic as the PRINT method to get
      // String representations for non-Compounds.
      rhsString = GET_PRINT_STRING(compound.rhs);
    }
    
    return "(" + lhsString + " " + compound.op + " " + rhsString + ")";
  }
  
  
  /**
   * Return a list of operators in this Compound, in the order which
   * they would be laid out in the expression this Compound represents.
   */
  def getOperatorsList(compound: Compound): List[String] = {
    
    // If this compound has no sub-compounds, then add it's operator
    // to our list and we're done.
    if (!isCompound(compound.lhs) && !isCompound(compound.rhs)) {
      
      return List(compound.op);
    }
    
    var lhsOpList: List[String] = List();
    var rhsOpList: List[String] = List();
    
    // Get operator list from left compound, if it is a compound.
    if (isCompound(compound.lhs)) {
      lhsOpList = getOperatorsList(compound.lhs.asInstanceOf[Compound]);
    }

    
    // Get operator list from right compound, if it is a compound.
    if (isCompound(compound.rhs)) {
      rhsOpList = getOperatorsList(compound.rhs.asInstanceOf[Compound]);
    }
    
    // We want (lhsOpList, current compound operator, rhsOpList).
    return (compound.op :: rhsOpList).:::(lhsOpList);
  }
  
  
  /**
   * Given a Compound, simplify all sub-Compounds which are two NumberValues.
   */
  def simplifyCompoundNumberValuePairs(compound: Compound): Value = {
    
    //println("GIVEN COMPOUND: " + compound);
    
    // Base case: both lhs and rhs are number values.
    if (isNumberValue(compound.lhs) && isNumberValue(compound.rhs)) {
      
      //println("DOING: " + compound.op + ", " + compound.lhs + ", " + compound.rhs);
      
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
    
    //println("OP:  " + compound.op);
    //println("LHS: " + compound.lhs);
    //println("RHS: " + compound.rhs);
    
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
      
      //println("DOING: " + compound.op + ", " + newLhs + ", " + newRhs);
      
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
    
    return simplify(Compound(op, newLhs, newRhs),approximate);
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
  
  
  /**
   * Returns true iff the given compound is made purely of NumberValues.
   */
  def allNumberValues(compound: Value): Boolean = compound match {
    case NumberValue(_,_) => true
    case Unbound(_) => false
    case Compound(op,v1:Value,v2:Value) => allNumberValues(v1) && allNumberValues(v2)
  }

  
  /**
   * Returns true iff the given Value is of type NumberValue
   */
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
  
  
  /**
   * Returns true iff the given Value is of type Compound.
   */
  def isCompound(value: Value): Boolean = value match {
    case c: Compound => true
    case otherwise => false
  }

  
  /**
   * Returns true iff the given Value is of type CompoundCluster.
   */
  def isCompoundClusterValue(value: Value): Boolean = value match {
    case c: CompoundCluster => true
    case otherwise => false
  }
  
  /**
   * Returns true iff the given Value is of type Unbound.
   */
  def isUnbound(value: Value): Boolean = value match {
    case u: Unbound => true
    case otherwise => false
  }
  
  
  //gets symbol out of unbound
  def getSym(value: Value): Symbol = value match {
    case Unbound(s) => s
    case otherwise => 'youwrong //your risk to call this on st that is not unbound
  }
  
}

