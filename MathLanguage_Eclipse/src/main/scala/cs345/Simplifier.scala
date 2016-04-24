/**
 * Methods and data structures dealing with simplification of Compound terms.
 */
object Simplifier {
  
  import MathCode._
  
  /*
   * Used an as intermediate data structure for simplifying compounds by Simplifier object. 
   * This should not shine through to the user--they should not know of this data structure.
   */
  case class CompoundCluster(ops: List[String], children: List[Value]) extends Value {
    
    override def toString: String = return Simplifier.flattenCompoundClusterToString(this) 
    
    // These are useless and should not be used for now.
    def + (rhs: Value): Value = null 
    def - (rhs: Value): Value = null 
    def * (rhs: Value): Value = null 
    def / (rhs: Value): Value = null 
    def ^ (rhs: Value): Value = null 
    def OVER (rhs: Value): Value = null 
  }
  

  
//COMPOUNDS: --------- --------- --------- --------- --------- --------- --------- --------- ---------  
  
  def simplifier(v:Value, binding:Map[Symbol, Value]):Value = {
    println("Simplifying this:")
    debug_print(v)
    println
    val v2 = simplifyCompound_wrapper(v, binding)
    //debug_print(v2)
    //println
    val v3 = v2 match {
      //v match {
      case NumberValue(n,d) => {
        //println(n+" "+d)
        if (n == 0 && d!=0) {
          return NumberValue(0,1)
        } else if (d == 1) {
          return NumberValue(n,d)
        } else {
          val g = gcd(n,d)
          return NumberValue(n / g, d / g)
        }
      }
      case Compound(op, lhs:NumberValue, rhs:NumberValue) => op match {
        case "+" => lhs + rhs
        case "-" => lhs - rhs
        case "*" => lhs * rhs
        case "/" => lhs / rhs
        case "^" => lhs ^ rhs
      }
      case Compound("^", NumberValue(IntBig(1),IntBig(1)), rhs) => {
        NumberValue(1,1)
      }
      case Compound("*", NumberValue(IntBig(0),_), rhs) => NumberValue(0,1)
      case Compound("*", lhs, NumberValue(IntBig(0),_)) => NumberValue(0,1)
      case Compound("*", NumberValue(IntBig(1),IntBig(1)), rhs) => rhs
      case Compound("*", lhs, NumberValue(IntBig(1),IntBig(1))) => lhs
      case Compound("+", lhs, NumberValue(IntBig(0),_)) => lhs
      case Compound("+", NumberValue(IntBig(0),_), rhs) => rhs
      case Compound("/", NumberValue(IntBig(0),_), rhs) => NumberValue(0,1)
      case Compound(outer_op, Compound(inner_op, lhs1, rhs1), rhs) => {
        val simp_lhs1 = simplify(lhs1, binding)
        val simp_rhs1 = simplify(rhs1, binding)
        val simp_rhs = simplify(rhs, binding)
        /*println("Simplifying "+outer_op+","+inner_op)
        debug_print(v)*/

        (outer_op, inner_op) match {
          case ("*", "+") | ("*", "-") => {
            val new_lhs = simplify(Compound("*", simp_lhs1, simp_rhs), binding)
            val new_rhs = simplify(Compound("*", simp_rhs1, simp_rhs), binding)
            /*println("new lhs, rhs:")
            debug_print(new_lhs)
            debug_print(new_rhs)*/
            simplify(Compound(inner_op, new_lhs, new_rhs), binding)
          }

          case otherwise => rhs match {
            case rhs_c:Compound => simplify_any_compound(outer_op, Compound(inner_op, lhs1, rhs1), rhs_c, false, binding)
            case otherwise => v2
          }
        }
      }

      // At this point, lhs is not a Compound
      case Compound(outer_op, lhs, Compound(inner_op, lhs1, rhs1)) => {
        //println("Hit Compound(op, something, Compound) case")
        simplify_any_compound(outer_op, lhs, Compound(inner_op, lhs1, rhs1), true, binding)
      }
      case otherwise => {
        //println("Hit otherwise case")
        v2
      }
    }
    combineUnbounds(v3)
  }
  
  
  def simplify_any_compound(outer_op:String, lhs:Value, c:Compound, recurse:Boolean, binding:Map[Symbol, Value]):Value = {
    val x = simplify_any_compound2(outer_op, lhs, c, binding)
    if (recurse && !x._1)
      simplify(x._2, binding)
    else
      x._2
  }

  def simplify_any_compound2(outer_op:String, lhs:Value, c:Compound, binding:Map[Symbol, Value]):(Boolean,Value) = c match {
    case Compound(inner_op, lhs1, rhs1) => {
      println("Simplifying:")
      debug_print(Compound(outer_op, lhs, c))

      (outer_op, inner_op) match {
        // Commutative operators
        case ("*",_) | ("+",_) =>
          (false,Compound(outer_op, Compound(inner_op, lhs1, rhs1), lhs))

        // a - (b - c) => (a - b) + c
        case ("-", "-") =>
          (false,Compound("+", simplify(Compound("-", lhs, lhs1), binding), simplify(rhs1, binding)))

        // Everything else
        case otherwise => (true,Compound(outer_op, lhs, c))
      }
    }
  }
  
  def simplifyCompound_wrapper(v:Value, binding:Map[Symbol, Value]):Value = v match {
    case c:Compound => simplifyCompound(c, binding)
    case otherwise => v
  }


  /**
    * Takes a compound, merges all of the same Unbounds:
    * a + a => 2*a
    * a*b + a*b => 2*a*b
    * 
    * We receive output from the above simplify() methods, so we don't have to
    * worry about really complicated stuff:
    * (a+1)*b, which becomes a*b+b
    */
  //def combineUnbounds(c: Compound): Value = {
  //}

  class UnboundSet(contents:Map[Unbound,Value]) {
    def add(u:Unbound, v:NumberValue) = {
      new UnboundSet(contents + (u -> v))
    }
    def getcontents = contents
    def absorb(other:UnboundSet) = {
      println(this + " is absorbing " + other)
      var newContents:Map[Unbound,Value] = Map()
      contents.keys.foreach{ k =>
        if (other.getcontents contains k) {
          newContents += (k -> (contents(k) + other.getcontents(k)))
        } else {
          newContents += (k -> contents(k))
        }
      }
      other.getcontents.keys.foreach{ k =>
        if (!(contents contains k)) {
          newContents += (k -> other.getcontents(k))
        }
      }
      new UnboundSet(newContents)
    }
    def toCompound(): Value = {
      var root:Value = NumberValue(1,1)
      var isFirst = true
      contents.keys.foreach{ k =>
        if (isFirst) {
          root = contents(k) match {
            case NumberValue(IntBig(1),IntBig(1)) => k
            case otherwise => Compound("^", k, contents(k))
          }
          isFirst = false
        } else {
          root = contents(k) match {
            case NumberValue(IntBig(1),IntBig(1)) =>
              Compound("*", root, k)
            case otherwise =>
              Compound("*", root, Compound("^", k, contents(k)))
          }
        }
      }
      println("Converted "+this+" to:")
      debug_print(root)
      println
      return root
    }
    override def toString(): String = {
      var s = ""
      contents.keys.foreach{ k =>
        s += k + ":" + contents(k) + ","
      }
      s
    }
    override def equals(other:Any): Boolean = other match {
      case c2:UnboundSet => {
        contents.keys.foreach{ k =>
          if (c2.getcontents contains k) {
            if (c2.getcontents(k) != contents(k)) {
              return false
            }
          } else {
            return false
          }
        }
        if (contents.size != c2.getcontents.size) return false
        return true
      }
      case otherwise => false
    }
    override def hashCode:Int = {
      return 5 // TODO: Pretty Much Random, should make this Not Terrible
    }
  }

  def combineUnbounds(v: Value): Value = {
    println("combineUnbounds called on:")
    debug_print(v)
    println
    val (_,m) = combineUnboundsRecursive(v)
    //m.toCompound()

    // Change m into a compound
    var root:Value = NumberValue(0,1)
    var isFirst = true
    m.keys.foreach{ k =>
      if (isFirst) {
        root = m(k) match {
          case NumberValue(IntBig(1),IntBig(1)) => k.toCompound()
          case otherwise => Compound("*", k.toCompound(), m(k))
        }
        isFirst = false
      } else {
        root = m(k) match {
          case NumberValue(IntBig(1),IntBig(1)) =>
            Compound("+", root, k.toCompound())
          case otherwise =>
            Compound("+", root, Compound("*", k.toCompound(), m(k)))
        }
      }
    }
    return root
  }

  def combineUnboundsRecursive(v: Value): (Value, Map[UnboundSet,Value]) = v match {
    case nv:NumberValue => (v,Map())
    case Compound("*",nv:NumberValue,rhs) => {
      val (lhs_v,lhs_m) = combineUnboundsRecursive(rhs)
      println("Number times a: "+lhs_m)
      if (lhs_m.size > 0)
        (v,Map(lhs_m.keysIterator.next() -> Compound("*", nv, lhs_m.valuesIterator.next())))
      else
        (v,Map())
    }
    case Compound("*",rhs,nv:NumberValue) => {
      val (lhs_v,lhs_m) = combineUnboundsRecursive(rhs)
      if (lhs_m.size > 0)
        (v,Map(lhs_m.keysIterator.next() -> Compound("*", nv, lhs_m.valuesIterator.next())))
      else
        (v,Map())
    }
    case Compound("*",lhs,rhs) => {
      val (lhs_v,lhs_m) = combineUnboundsRecursive(lhs)
      val (rhs_v,rhs_m) = combineUnboundsRecursive(rhs)

      // Each map should only have a single entry, or we're hosed
      if (lhs_m.size != 1 || rhs_m.size != 1) {
        debug_print(lhs_v)
        println
        println(lhs_m)
        debug_print(rhs_v)
        println
        println(rhs_m)
        throw new Exception("lhs and rhs maps should only have 1 entry each! But probably this code can be implemented.")
      }

      // Combine the keys, add the values (remember: Each one only has 1).
      val newkey = lhs_m.keysIterator.next().absorb(rhs_m.keysIterator.next())
      (v,Map(newkey -> Compound("*", lhs_m.valuesIterator.next(), rhs_m.valuesIterator.next())))
    }
    case Compound("+",lhs,rhs) => {
      val (lhs_v,lhs_m) = combineUnboundsRecursive(lhs)
      val (rhs_v,rhs_m) = combineUnboundsRecursive(rhs)
      println("Adding "+lhs_m+" to "+rhs_m)

      // Merge the two: Essentially concatenation
      var res_m:Map[UnboundSet,Value] = Map()
      lhs_m.keys.foreach{ k =>
        println(k)
        if (rhs_m contains k) {
          // Both lhs and rhs have the given key, add them
          println("both")
          //println("both: "+lhs_m(k)+" and "+rhs_m(k)+" go to "+(lhs_m(k)+rhs_m(k)))
          res_m += (k -> (lhs_m(k) + rhs_m(k)))
        } else {
          println("only lhs")
          // Only lhs has the key
          res_m += (k -> lhs_m(k))
        }
      }
      rhs_m.keys.foreach{ k =>
        if (!(lhs_m contains k)) {
          println(k+" only in rhs")
          res_m += (k -> rhs_m(k))
        }
      }
      println(res_m)
      (v,res_m)
    }
    case Compound("^",lhs,nv:NumberValue) => {
      val (lhs_v,lhs_m) = combineUnboundsRecursive(lhs)
      var res_m:Map[UnboundSet,Value] = Map()
      lhs_m.keys.foreach{ k =>
        res_m += (k -> (lhs_m(k) * nv))
      }
      println("Exponential to a NV gives: "+res_m+" from: "+lhs_m)
      (v,res_m)
    }
    case Compound(op,_,_) => {
      throw new Exception("Haven't implemented operator "+op+" yet")
      //(v,Map())
    }
    case u:Unbound => {
      var us = new UnboundSet(Map(u -> NumberValue(1,1)))
      (v,Map(us -> NumberValue(1,1)))
    }
    case otherwise => (v,Map())
  }
  
  
  /**
   * Simplifies the given Compound, returns a CompoundCluster.
   */
  def simplifyCompoundtoCompoundCluster(compound: Compound, binding:Map[Symbol, Value]): Value = {
    
    // Replace all variables by their bindings.
    var tempValue: Value = getCompoundGivenBinding(compound, binding) 
    
    // If the result is not a Compound, then return it. It could be a
    // NumberValue or an Unbound, for example.
    if (isNumberValue(tempValue) || isUnbound(tempValue)) {
      return tempValue 
    }
    
    // Else, it is a compound, so cast it. Check just to make sure.
    if (!isCompound(tempValue)) {
      println("ERROR: simplifyCompound") 
      return null 
    }
    
    var tempCompound: Compound = tempValue.asInstanceOf[Compound]
    
    // Simplify all pairs of two NumberValues.
    tempValue = simplifyCompoundNumberValuePairs(tempCompound) 
    
    // If we were able to generate a single NumberValue from the first
    // step of simplification (this means all values in the Compound were
    // NumberValues). Else, it returns a Compound.
    if (!isCompound(tempValue)) {
      return tempValue 
    }
    
    // Else, we will create a CompoundCluster, merge its groups, and simplify
    // its groups. We know it is a Compound, so casting is fine here.
    var cc: CompoundCluster = compoundToCompoundCluster(tempValue.asInstanceOf[Compound]) 
    cc = mergeGroups(cc.ops(0), cc.children(0), cc.children(1)) 
    var finalResult: Value = simplifyGroups(cc) 
    
    return finalResult 
  }
  
  
  /**
   * Simplifies the given Compound, returns a Compound.
   */
  def simplifyCompound(compound: Compound, binding: Map[Symbol, Value]): Value = {
    
    // Replace all variables by their bindings.
    var tempValue: Value = getCompoundGivenBinding(compound, variableMap) 
    
    // If the result is not a Compound, then return it. It could be a
    // NumberValue or an Unbound, for example.
    if (isNumberValue(tempValue) || isUnbound(tempValue)) {
      return tempValue 
    }
    
    // Else, it is a compound, so cast it. Check just to make sure.
    if (!isCompound(tempValue)) {
      println("ERROR: simplifyCompound") 
      return null 
    }
    
    var tempCompound: Compound = tempValue.asInstanceOf[Compound]
    
    // Simplify all pairs of two NumberValues.
    tempValue = simplifyCompoundNumberValuePairs(tempCompound) 
    
    // If we were able to generate a single NumberValue from the first
    // step of simplification (this means all values in the Compound were
    // NumberValues). Else, it returns a Compound.
    if (!isCompound(tempValue)) {
      return tempValue 
    }
    
    // Else, we will create a CompoundCluster, merge its groups, and simplify
    // its groups. We know it is a Compound, so casting is fine here.
    var cc: CompoundCluster = compoundToCompoundCluster(tempValue.asInstanceOf[Compound]) 
    cc = mergeGroups(cc.ops(0), cc.children(0), cc.children(1)) 
    var finalResult: Value = simplifyGroups(cc) 
    
    // If the final result is not a CompoundCluster.
    if (isNumberValue(finalResult) || isUnbound(finalResult)) {
      return finalResult 
    }
    
    // Else, the final result is a CompoundCluster. Map it to a Compound
    // and return it.
    finalResult = compoundClusterToCompound(finalResult.asInstanceOf[CompoundCluster]) 
    
    return finalResult 
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
      var lhsCC = lhs.asInstanceOf[CompoundCluster] 
      var newLhs = mergeGroups(lhsCC.ops(0), lhsCC.children(0), lhsCC.children(1)) 
      
      // Try to merge rhs recursively
      var rhsCC = rhs.asInstanceOf[CompoundCluster] 
      var newRhs = mergeGroups(rhsCC.ops(0), rhsCC.children(0), rhsCC.children(1)) 
      
      // Get precedence levels of the lhs group, the rhs group, and the 
      // operator trying to connect them.
      var lhsPrecedenceLevel: Int = precedenceMap(newLhs.ops(0)) 
      var opPrecedenceLevel: Int = precedenceMap(op) 
      var rhsPrecedenceLevel: Int = precedenceMap(newRhs.ops(0)) 
      
      // We can merge the groups iff all precedence levels are equal.
      if ((lhsPrecedenceLevel == opPrecedenceLevel) && (opPrecedenceLevel == rhsPrecedenceLevel)) {
        
        // If in the format: (lhs - rhs), and the rhs is made up of only
        // + and - operators (as it is here), we will convert each +
        // in the rhs operator list to a -, and convert each + to a -.
        var alternateNewRhsOps: List[String] = List() 
        
        if (op.equals("-")) {
          
          for (element <- newRhs.ops) {  
            var newElement = element match {
              case "+" => "-" 
              case "-" => "+" 
            }
            
            alternateNewRhsOps = alternateNewRhsOps.:+(newElement) 
          }
          
        }
        else {
          // Common case.
          alternateNewRhsOps = newRhs.ops 
        }
        
        //println("newRHS.ops: " + newRhs.ops) 
        //println("alternateNewRhsOps: " + alternateNewRhsOps) 
        
        // For ops list we want: (lhs.ops, op, rhs.ops).
        var newOps: List[String] = newLhs.ops :+ op 
        newOps = newOps ::: alternateNewRhsOps 
        
        // For children list we want: (lhs.children, rhs.children).
        var newChildren: List[Value] = newLhs.children ::: newRhs.children 
        
        return new CompoundCluster(newOps, newChildren) 
      }
      // Else, we could not merge them into one, so return the original
      // format of (lhs <op> rhs), but with lhs and rhs possibly
      // optimized (i.e., subparts were merged recursively).
      else {
        return new CompoundCluster(List(op), List(newLhs, newRhs)) 
      }
    }
    
    // Case 2:
    if (isCompoundClusterValue(lhs) && !isCompoundClusterValue(rhs)) {
      
      // Try to merge the lhs recursively.
      var lhsCC = lhs.asInstanceOf[CompoundCluster] 
      var newLhs = mergeGroups(lhsCC.ops(0), lhsCC.children(0), lhsCC.children(1)) 
      
      var lhsPrecedenceLevel: Int = precedenceMap(newLhs.ops(0)) 
      var opPrecedenceLevel: Int = precedenceMap(op) 
      
      if (lhsPrecedenceLevel == opPrecedenceLevel) {
        
        // For ops list we want: (lhs.ops, op).
        var newOps: List[String] = newLhs.ops :+ op 
        
        // For children list we want: (lhs.children, rhs).
        var newChildren: List[Value] = newLhs.children :+ rhs 
        
        return new CompoundCluster(newOps, newChildren) 
      }
      // Else, we could not merge them into one, so return the original
      // format of (lhs <op> rhs), but with lhs and rhs possibly
      // optimized (i.e., subparts were merged recursively).
      else {
        return new CompoundCluster(List(op), List(newLhs, rhs)) 
      }
    }
    
    // Case 3:
    if (!isCompoundClusterValue(lhs) && isCompoundClusterValue(rhs)) {
      
      // Try to merge rhs recursively
      var rhsCC = rhs.asInstanceOf[CompoundCluster] 
      var newRhs = mergeGroups(rhsCC.ops(0), rhsCC.children(0), rhsCC.children(1)) 
      
      var rhsPrecedenceLevel: Int = precedenceMap(newRhs.ops(0)) 
      var opPrecedenceLevel: Int = precedenceMap(op) 
      
      if (rhsPrecedenceLevel == opPrecedenceLevel) {
        
        // If in the format: (lhs - rhs), and the rhs is made up of only
        // + and - operators (as it is here), we will convert each +
        // in the rhs operator list to a -, and convert each + to a -.
        var alternateNewRhsOps: List[String] = List() 
        
        if (op.equals("-")) {
          
          for (element <- newRhs.ops) {  
            var newElement = element match {
              case "+" => "-" 
              case "-" => "+" 
            }
            
            alternateNewRhsOps = alternateNewRhsOps.:+(newElement) 
          }
          
        }
        else {
          // Common case.
          alternateNewRhsOps = newRhs.ops 
        }
        
        // For ops list we want: (op, rhs.ops).
        var newOps: List[String] = alternateNewRhsOps.+:(op) 
        
        // For children list we want: (lhs, rhs.children).
        var newChildren: List[Value] = newRhs.children.+:(lhs) 
        
        return new CompoundCluster(newOps, newChildren) 
      }
      // Else, we could not merge them into one, so return the original
      // format of (lhs <op> rhs), but with lhs and rhs possibly
      // optimized (i.e., subparts were merged recursively).
      else {
        return new CompoundCluster(List(op), List(lhs, newRhs)) 
      }
    }
    
    // Case 4:
    if (!isCompoundClusterValue(lhs) && !isCompoundClusterValue(rhs)) {
      
      // Merge the terminal values.
      return new CompoundCluster(List(op), List(lhs, rhs)) 
    }
    
    // Should never get here.
    return null 
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
    var newChildren: List[Value] = compoundCluster.children 
    var newOps: List[String] = compoundCluster.ops 
    
    // We must simplify from the leaves up, since we can only simplify groups,
    // and groups are leaf nodes. Only simplify if none of the children of a 
    // given node are CompoundClusters (i.e., only simplify leaf nodes).
    
    // If any children are CompoundCluster type, simplify them recursively before
    // we analyze this group.
    var elementIndex: Int = 0 
    for (element: Value <- compoundCluster.children) {
    
      if (isCompoundClusterValue(element)) {
        
        // Recursively simplify this child's groups.
        var simplifiedChild: Value = simplifyGroups(element.asInstanceOf[CompoundCluster]) 
        newChildren = newChildren.updated(elementIndex, simplifiedChild) 
      }
      
      elementIndex += 1 
    }
    
    //***********************************
    //* Simpilify the given group below.
    //***********************************
    
    //println("CURRENT GROUP TO SIMPLIFY: " + new CompoundCluster(newOps, newChildren))  
    
    //******************************************************************
    //* Try to find matching Unbound objects of the couple {a, -a}.
    //* If we find one, cancel them out. For now, we do not do anything
    //* in terms of making something of the form {a, a} = {2a}.
    //******************************************************************
    var stillSimplyfingUnbounds: Boolean = true 
    
    while (stillSimplyfingUnbounds) {
      
      //println("NEW CHILDREN: " + newChildren) 
      //println("NEW OPS: " + newOps) 
      
      var varName: Symbol = null 
      var varFound1: Boolean = false 
      var varFound2: Boolean = false 
      var positive1: Boolean = false 
      var position1: Int = -1 
      var positive2: Boolean = false 
      var position2: Int = -1 
      var currElementIndex : Int = 0 
    
      // Traverse through children and try to find matching elements children
      // of the form {v, -v} where a is an Unbound.
      for (element: Value <- newChildren) {
      
        // If this element is an Unbound, check it out.
        if (isUnbound(element)) {
        
          // If we are still looking for our first Unbound (v).
          if (!varFound1) {
          
            // If we haven't found one yet, use this one.
            // Note it's symbol and position in the children list.
            varFound1 = true 
            varName = element.asInstanceOf[Unbound].sym 
            position1 = currElementIndex 
          
            // Find whether this Unbound is positive or negative.
            if (position1 == 0) {
              // If there are no operators before it, then it is a positive
              // unbound.
              positive1 = true 
            }
            else {
              // If there are operators before it, then it is a positive unbound
              // iff the operator is not a minus sign.
              positive1 = !(newOps(currElementIndex - 1) == "-") 
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
                position2 = currElementIndex 
              
                // Check if it is the opposite sign. If so, we found our 
                // second var and we can stop looking, we will cancel them 
                // out after the loop is done iterating. It cannot be the 
                // first child by definition, since this is the second 
                // variable, so it is safe to assume it has a sign before it.
                positive2 = !(newOps(currElementIndex - 1) == "-") 
              
                // If the first and the second variable are opposite signs,
                // we can cancel them.
                if (positive1 != positive2)
                {
                  varFound2 = true 
                  
                  // We should break here, but it's complex and ugly in Scala, so
                  // the loop will iterate fully, but no longer look for any
                  // matching variables  we've found our two for this cycle of
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
      
        currElementIndex += 1 
      }
    
      //****************************************************
      //* If we found matching variables, cancel them out.
      //****************************************************
      if (varFound1 && varFound2) {
        
        stillSimplyfingUnbounds = true 
        
        //println("FOUND SOME TO DELETE: " + varName) 
        //println("position1: " + position1) 
        //println("position2: " + position2) 
    
        // Remove the first Unbound from the group.
        newChildren = newChildren.take(position1) ++ newChildren.drop(position1 + 1) 
    
        // Remove the second Unbound from the group. Note: we know that the second
        // position is greater than the first position, so we are okay subtracting
        // one from the second position--there is no chance of out of bounds.
        newChildren = newChildren.take(position2 - 1) ++ newChildren.drop(position2) 
    
        //println("NEW CHILDREN: " + newChildren) 
    
        // Remove the first operator from the group. We want to remove the operator
        // to the direct left of the Unbound we removed. If we removed the first
        // Unbound in a group, then we must remove the same index operator.
        if (position1 == 0) {
      
          newOps = newOps.take(position1) ++ newOps.drop(position1 + 1) 
          newOps = newOps.take(position2 - 2) ++ newOps.drop(position2 - 1) 
        }
        // Else, we want to remove the operator at the index before whatever the
        // Unbound was at.
        else {
      
          newOps = newOps.take(position1 - 1) ++ newOps.drop(position1) 
          newOps = newOps.take(position2 - 2) ++ newOps.drop(position2 - 1) 
        }
    
        // If we removed the first two children, there will be an extra 
        // operator to the far left, since we have wiped out the first 
        // two Values and we only remove the operators to the left
        // of removed Values in the general case.
        if ((position1 == 0) && (position2 == 1)) {
        
          newOps = newOps.drop(1) 
        }
    
          //println("NEW OPS: " + newOps) 
      }
      else {
        
        stillSimplyfingUnbounds = false 
      }
    } // Check if there exists another pair of matching variables (while loop).
    
    
    // Special condition: if the expression was purely Unbounds and they 
    // all cancelled, make the result 0.
    if (newChildren.length == 0) {
      
      newChildren = List(NumberValue(0,1)) 
    }
    
    //*************************************************************************
    //* At this point, all Unbounds are simplified for the given group. Now, 
    //* simplify NumberValues in the given group.
    //*************************************************************************
    
    var stillSimplifyingNumbers: Boolean = true 
    
    while (stillSimplifyingNumbers) {
    
      // Simplify numbers. We are able to traverse the group one time, keeping
      // a running sum of the NumberValues. We will leave the Unbounds in their
      // place if we come across them, since they have already been taken care
      // of.
      var currLHS: NumberValue = null 
      var currRHS: NumberValue = null 
      var currIndex: Int = 0 
      var lhsIndex: Int = -1 
      var rhsIndex: Int = -1 
      var currOp: String = null 
      var foundLHS: Boolean = false 
      var foundRHS: Boolean = false 
    
      for (element: Value <- newChildren) {
      
        // If not a NumberValue, skip this element.
        if (isNumberValue(element)) {
        
          // If we have found a LHS already.
          if (foundLHS) {
            
            // If we have not found a RHS, but have found a LHS.
            if (!foundRHS) {
          
              foundRHS = true 
          
              // Store information about it to use later.
              
              // We have found a RHS. We will take the LHS and the RHS,
              // do the appropriate operation on them, and store the result
              // in the place of the LHS. Thus, a final NumberValue result
              // will remain in the LHS index at the end of the computation
              // of joining NumberValues.
              currRHS = element.asInstanceOf[NumberValue] 
              rhsIndex = currIndex 
          
              // Note that the RHS cannot be the first child, because if the first
              // child were a NumberValue, it would have been taken by the LHS.
              // Thus, the operator before the RHS is always at the same index
              // relative to the index of the RHS.
              currOp = newOps(currIndex - 1) 
              
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
            currLHS = element.asInstanceOf[NumberValue] 
            lhsIndex = currIndex 
            foundLHS = true 
        
            // We now have all necessary information for the LHS. Find a RHS.
            
            }
        }
      
        currIndex += 1 
      }
    
      // Once we've found a LHS and RHS, do the computation and remove
      // the proper elements from the children and ops list.
      if (foundLHS && foundRHS) {
        
        // Remove the RHS and its operator from their lists respectively.
        // Because of the nature of the RHS, the indices to remove are
        // straight forward.
        newOps = newOps.take(rhsIndex - 1) ++ newOps.drop(rhsIndex) 
        newChildren = newChildren.take(rhsIndex) ++ newChildren.drop(rhsIndex + 1) 
          
        //println("NEW OPS:      " + newOps) 
        //println("New CHILDREN: " + newChildren) 
          
        // Perform the operation.
        var newValue: Value = currOp match {
          
          case "+" => currLHS.+(currRHS) 
          case "-" => currLHS.-(currRHS) 
          case "*" => currLHS.*(currRHS) 
          case "/" => currLHS./(currRHS) 
          case "^" => currLHS.^(currRHS) 
        }
          
        var newNumberValue: NumberValue = newValue.asInstanceOf[NumberValue] 
          
        //println("NEW NUMBER VALUE: " + newNumberValue) 
          
        // Replace the LHS with the result.
        newChildren = newChildren.updated(lhsIndex, newNumberValue) 
      }
      
      // We are done simplifying NumberValues.
      else {
        stillSimplifyingNumbers = false 
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
        
        return newChildren(0) 
      }
      else if (isNumberValue(newChildren(0))) {
        
        return newChildren(0) 
      }
    }
    
    // Return the new CompoundCluster.
    return new CompoundCluster(newOps, newChildren) 
  }
  
  
  /**
   * Converts a CompoundCluster to a Compound.
   */
  def compoundClusterToCompound(cc: CompoundCluster): Compound = {
    
    // If this is a leaf CompundCluster, then all of the children are
    // NumberValues or Unbounds. In this case, convert to a Compound and return
    // this Compound.
    
    // If any of the children are CompoundClusters, then we know we need to simplify
    // each of them.
    var newChildren: List[Value] = List() 
    
    //println("\n NEW SET: ") 
    //println(cc.children) 
    //println(cc.ops) 
    
    for (element <- cc.children)
    {
      if (isCompoundClusterValue(element)) {
        
        //println("ELEMENT: " + element) 
        
        var newChild: Compound = compoundClusterToCompound(element.asInstanceOf[CompoundCluster]) 
        newChildren = newChildren.:+(newChild) 
      }
      else {
        newChildren = newChildren.:+(element) 
      }
    }
    
    //println("All children are taken care of, newChildren = " + newChildren) 
    
    // Now combine all children into a left-associative Compound structure and
    // return it.
    return groupToCompound(newChildren, cc.ops) 
  }
  
  
  /**
   * Given a group of NumberValues and Unbounds, returns the proper
   * left-associative Compound for this group.
   */
  def groupToCompound(values: List[Value], operators: List[String]): Compound = {
    
    //println("List of operators: " + operators) 
    //println("List of values: " + values) 
    
    // Base case:
    if (values.length == 2) {
      return new Compound(operators(0), values(0), values(1)) 
    }
    
    var valLength: Int = values.length 
    var opsLength: Int = operators.length 
    
    return new Compound(operators(opsLength - 1), groupToCompound(values.take(valLength - 1), operators.take(opsLength - 1)), values(valLength - 1)) 
  }
  
  
  /**
   * Converts a Compound to a CompoundCluster.
   */
  def compoundToCompoundCluster(compound: Compound): CompoundCluster = {
    
    // Base case: neither the LHS or RHS are compounds. Thus, this
    // is a leaf compound. Convert it to a CompoundCluster.
    if (!isCompound(compound.lhs) && !isCompound(compound.rhs)) {
      return new CompoundCluster(List(compound.op), List(compound.lhs, compound.rhs)) 
    }
    
    // Either LHS or RHS (or both) are Compounds. Create CompoundClusters
    // with them.
    var lhsIsCompoundCluster: Boolean = false 
    var rhsIsCompoundCluster: Boolean = false 
    var lhsCompoundCluster: CompoundCluster = null 
    var rhsCompoundCluster: CompoundCluster = null 
    
    // If LHS is Compound, convert it to a CompoundCluster.
    if (isCompound(compound.lhs)) {
      lhsCompoundCluster = compoundToCompoundCluster(compound.lhs.asInstanceOf[Compound]) 
      lhsIsCompoundCluster = true 
    }
    
    // If RHS is Compound, convert it to a CompoundCluster.
    if (isCompound(compound.rhs)) {
      rhsCompoundCluster = compoundToCompoundCluster(compound.rhs.asInstanceOf[Compound]) 
      rhsIsCompoundCluster = true 
    }
    
    // If both RHS and LHS are now CompoundClusters, return a CompoundCluster.
    // composed of them.
    if (lhsIsCompoundCluster && rhsIsCompoundCluster) {
      return new CompoundCluster(List(compound.op), List(lhsCompoundCluster, rhsCompoundCluster)) 
    }
    
    // Either just RHS is a CompoundCluster, or just LHS is a CompoundCluster
    // at this point.
    
    if (lhsIsCompoundCluster) {
      return new CompoundCluster(List(compound.op), List(lhsCompoundCluster, compound.rhs)) 
    }
    
    if (rhsIsCompoundCluster) {
      return new CompoundCluster(List(compound.op), List(compound.lhs, rhsCompoundCluster)) 
    }
    
    // Should never be here.
    return null 
  }
  
  
  /**
   * Flatten the given CompoundCluster to a fully-parenthesized String.
   */
  def flattenCompoundClusterToString(compoundCluster: CompoundCluster): String = {
    
    // Base case: none of the children are CompoundClusters. Thus, this
    // is a leaf CompoundCluster.
    var allChidrenNotCompoundCluster: Boolean = false 
    
    for (element: Value <- compoundCluster.children) {
      if (isCompoundClusterValue(element)) {
        allChidrenNotCompoundCluster = false 
      }
    }
    
    // This is the base case. Return a String representing this leaf 
    // CompoundCluster.
    if (allChidrenNotCompoundCluster) {
      
      var returnString: String = "" 
      
      returnString += "(" 
      
      var currentOpIndex: Int = 0 
      
      for (element: Value <- compoundCluster.children) {
        
        returnString += element.toString() 
        
        // If we still have operators to print.
        if (currentOpIndex < compoundCluster.ops.length) {
          returnString += " " + compoundCluster.ops(currentOpIndex) + " " 
        }
        
        // Move on to the next operator.
        currentOpIndex = currentOpIndex + 1 
      }
      
      returnString += ")" 
      
    }
    
    // Else, at least one of the children is a CompoundCluster which needs to
    // be simplified recursively by this method.
    var returnString = "(" 
    var currentOpIndex: Int = 0 
    
    // For each child, if it is a Compound Cluster, recursively flatten it to
    // a String, else just use the child's value.
    for (element: Value <- compoundCluster.children) {
      
      // If this child is a CompoundCluster.
      if (isCompoundClusterValue(element)) {
        returnString += flattenCompoundClusterToString(element.asInstanceOf[CompoundCluster]) 
      }
      // Else it's not, just take its Value.
      else {
        returnString += element.toString() 
      }
      
      // If we still have operators to print.
      if (currentOpIndex < compoundCluster.ops.length) {
          returnString += " " + compoundCluster.ops(currentOpIndex) + " " 
      }
      
      // Move on to the next operator.
      currentOpIndex = currentOpIndex + 1 
    }
    
    returnString += ")" 
    
    return returnString 
  }
  
  
  /**
   * Flatten the given Compound to a fully-parenthesized String.
   */
  def flattenCompoundToString(compound: Compound): String = {
    
    // Base case: neither the LHS or RHS are compounds. Thus, this
    // is a leaf compound.
    if (!isCompound(compound.lhs) && !isCompound(compound.rhs)) {
      return "(" + compound.lhs.toString() + " " + compound.op + " " + compound.rhs.toString() + ")" 
    }
    
    // Either the LHS or the RHS (or both) are Compounds. Get their strings
    // recursively. Note that compounds are left-associative.
    var lhsString: String = "" 
    var rhsString: String = "" 
    
    // If LHS is compound, get its string.
    if (isCompound(compound.lhs)) {
      lhsString = flattenCompoundToString(compound.lhs.asInstanceOf[Compound]) 
    }
    // Else, it's not a compound, get it's string with its toString method.
    else {
      // Note: for now we are using the same logic as the PRINT method to get
      // String representations for non-Compounds.
      lhsString = compound.lhs.toString() 
    }
    
    // If RHS is compound, get its string.
    if (isCompound(compound.rhs)) {
      rhsString = flattenCompoundToString(compound.rhs.asInstanceOf[Compound]) 
    }
    // Else, it's not a compound, get it's string with its toString method.
    else {
      // Note: for now we are using the same logic as the PRINT method to get
      // String representations for non-Compounds.
      rhsString = compound.rhs.toString() 
    }
    
    return "(" + lhsString + " " + compound.op + " " + rhsString + ")" 
  }
  
  
  /**
   * Return a list of operators in this Compound, in the order which
   * they would be laid out in the expression this Compound represents.
   */
  def getOperatorsList(compound: Compound): List[String] = {
    
    // If this compound has no sub-compounds, then add it's operator
    // to our list and we're done.
    if (!isCompound(compound.lhs) && !isCompound(compound.rhs)) {
      
      return List(compound.op) 
    }
    
    var lhsOpList: List[String] = List() 
    var rhsOpList: List[String] = List() 
    
    // Get operator list from left compound, if it is a compound.
    if (isCompound(compound.lhs)) {
      lhsOpList = getOperatorsList(compound.lhs.asInstanceOf[Compound]) 
    }

    
    // Get operator list from right compound, if it is a compound.
    if (isCompound(compound.rhs)) {
      rhsOpList = getOperatorsList(compound.rhs.asInstanceOf[Compound]) 
    }
    
    // We want (lhsOpList, current compound operator, rhsOpList).
    return (compound.op :: rhsOpList).:::(lhsOpList) 
  }
  
  
  /**
   * Given a Compound, simplify all sub-Compounds which are two NumberValues.
   */
  def simplifyCompoundNumberValuePairs(compound: Compound): Value = {
    
    //println("GIVEN COMPOUND: " + compound) 
    
    // Base case: both lhs and rhs are number values.
    if (isNumberValue(compound.lhs) && isNumberValue(compound.rhs)) {
      
      //println("DOING: " + compound.op + ", " + compound.lhs + ", " + compound.rhs) 
      
      return compound.op match {
        case "+" => compound.lhs.+(compound.rhs) 
        case "-" => compound.lhs.-(compound.rhs) 
        case "*" => compound.lhs.*(compound.rhs) 
        case "/" => compound.lhs./(compound.rhs) 
        case "^" => compound.lhs.^(compound.rhs) 
      }
    }
    
    // We will try to simplify the lhs and the rhs as much as we can.
    var newLhs = compound.lhs 
    var newRhs = compound.rhs 
    
    //println("OP:  " + compound.op) 
    //println("LHS: " + compound.lhs) 
    //println("RHS: " + compound.rhs) 
    
    // If the lhs is a compound, try to simplify it.
    if (isCompound(compound.lhs)) {
      newLhs = simplifyCompoundNumberValuePairs(compound.lhs.asInstanceOf[Compound]) 
    }
    
    // If the rhs is a compound, try to simplify it.
    if (isCompound(compound.rhs)) {
      newRhs = simplifyCompoundNumberValuePairs(compound.rhs.asInstanceOf[Compound]) 
    }
    
    //println("NEW LHS: " + newLhs) 
    //println("NEW RHS: " + newRhs) 
    
    // Once simplified, if both are number values, then we can return a number value.
    if (isNumberValue(newLhs) && isNumberValue(newRhs)) {
      
      //println("DOING: " + compound.op + ", " + newLhs + ", " + newRhs) 
      
      return compound.op match {
        case "+" => newLhs.+(newRhs) 
        case "-" => newLhs.-(newRhs) 
        case "*" => newLhs.*(newRhs) 
        case "/" => newLhs./(newRhs) 
        case "^" => newLhs.*(newRhs) 
      }
    }
    
    // They are not both number values, so return a compound.
    return Compound(compound.op, newLhs, newRhs) 
  }
  
  
  /**
   * Returns true iff the given Value is of type CompoundCluster.
   */
  def isCompoundClusterValue(value: Value): Boolean = value match {
    case c: CompoundCluster => true
    case otherwise => false
  }
  
  
  /**
   * Given a Compound and a Binding, return a new Compound in which all unbound
   * variables are replaced by their bindings, if such a binding
   * exists.
   */
  def getCompoundGivenBinding(compound: Compound, binding:Map[Symbol, Value]): Compound = {
    
    // The final new lhs and rhs for this compound. These are
    // built recursively.
    var newLhs: Value = null
    var newRhs: Value = null
    var op: String = compound.op
    
    newLhs = compound.lhs match {
      case compound:Compound => getCompoundGivenBinding(compound, binding)
      case numberValue:NumberValue => numberValue
      case Unbound(unboundSymbol) => variableLookupFromBinding(unboundSymbol, binding)
    }
    
    newRhs = compound.rhs match {
      case compound:Compound => getCompoundGivenBinding(compound, binding)
      case numberValue:NumberValue => numberValue
      case Unbound(unboundSymbol) => variableLookupFromBinding(unboundSymbol, binding)
    }
    
    return Compound(op, newLhs, newRhs) 
  }


 
  //Returns the greatest common divisor of a and b
  def gcd(a:BigInt, b:BigInt):BigInt = {
    if (a==0 || b==0) return 1
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
  
  
}

