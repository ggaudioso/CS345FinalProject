

object Test {
  
  import MathCode._
  import Simplifier._
 
  
  def main(args: Array[String]):Unit = {  
    test()
  }
  
  
  def testIfEqual(a: String, b:String): Boolean = {
    if (a.equals(b)) {
      println(" PASS") 
      return true 
    }
    else {
      println(" FAIL") 
      return false 
    }
  }
  
  /**
   * Arbitrary test method.
   */
  def test() {
    
    var compound1 = Compound("+", Unbound('mike), NumberValue(3,1)) 
    var compound2 = Compound("-", NumberValue(4,1), Unbound('mike)) 
    var compound3 = Compound("-", compound1, compound2) 
    var compound4 = Compound("+", NumberValue(55,1), compound1) 
    var compound5 = Compound("-", compound4, compound2) 
    var compound6 = Compound("+", compound1, compound2) 
    var longCompoundTest = Compound("+", Compound("-", Unbound('mike), Unbound('mike)), Compound("-", Unbound('mike), Unbound('mike))) 
    
    // This is 'x := 'a + 'b + 'c
    var test = Compound("+",Compound("+",Unbound('mike),Unbound('james)),Unbound('mike)) 
    
    // This is 'x := 'a + 'b + 'c + 'd
    var test2 = Compound("+",Compound("+",Compound("+",Unbound('mike),Unbound('james)),Unbound('taylorSwift)),Unbound('selenaGomez)) 

    var test3 = Compound("-", test, test2) 
    var test4 = Compound("*", compound3, compound5) 
    var test5 = Compound("*", compound6, compound5) 
    var test6 = Compound("^", Compound("-", Compound("+", 'mike, 1), 'mike), 3) 
    var test7 = Compound("^", Compound("-", Compound("+", 'mike, 1), 'mike), 'james) 
    
    println("\nTESTING CONVERT TO COMPOUND CLUSTER: " ) 
    var testResults: List[Boolean] = List() 
    
    var cc3: CompoundCluster = compoundToCompoundCluster(compound3) 
    // ((mike + 3) - (4 - mike))
    print(flattenCompoundClusterToString(cc3)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc3), "((mike + 3) - (4 - mike))")) 
    
    var cc4: CompoundCluster = compoundToCompoundCluster(compound5) 
    // ((55 + (mike + 3)) - (4 - mike))
    print(flattenCompoundClusterToString(cc4)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc4), "((55 + (mike + 3)) - (4 - mike))")) 
    
    var cc5: CompoundCluster = compoundToCompoundCluster(longCompoundTest) 
    // ((mike - mike) + (mike - mike))
    print(flattenCompoundClusterToString(cc5)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc5), "((mike - mike) + (mike - mike))")) 
    
    var cc6: CompoundCluster = compoundToCompoundCluster(test4) 
    // (((mike + 3) - (4 - mike)) * ((55 + (mike + 3)) - (4 - mike)))
    print(flattenCompoundClusterToString(cc6)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc6), "(((mike + 3) - (4 - mike)) * ((55 + (mike + 3)) - (4 - mike)))")) 
    
    var cc7: CompoundCluster = compoundToCompoundCluster(test5) 
    // (((mike + 3) + (mike - a)) * ((55 + (mike + 3)) - (4 - mike)))
    print(flattenCompoundClusterToString(cc7)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc7), "(((mike + 3) + (4 - mike)) * ((55 + (mike + 3)) - (4 - mike)))")) 
    
    var cc8: CompoundCluster = compoundToCompoundCluster(test6) 
    // (((mike + 1) - mike) ^ 3)
    print(flattenCompoundClusterToString(cc8)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc8), "(((mike + 1) - mike) ^ 3)")) 
    
    var cc9: CompoundCluster = compoundToCompoundCluster(test7) 
    // (((mike + 1) - mike) ^ james)
    print(flattenCompoundClusterToString(cc9)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc9), "(((mike + 1) - mike) ^ james)")) 
    
    // (((mike + james) + taylorSwift) + selenaGomez)
    var cc10: CompoundCluster = compoundToCompoundCluster(test2) 
    print(flattenCompoundClusterToString(cc10)) 
    testResults = testResults.:+(testIfEqual(flattenCompoundClusterToString(cc10), "(((mike + james) + taylorSwift) + selenaGomez)")) 
    
    for (element <- testResults) {
      if (!element) {
        println("\n\t--------------------------------------") 
        println("\t- Converting to CC tests had a FAILURE") 
        println("\t--------------------------------------") 
        return 
      }
    }
    
    //********************************
    //* Testing merge values function
    //********************************
    
    println("\nTESTING MERGE GROUPS FUNCTION: ") 
    
    // (mike + 3 - 4 + mike)
    var group1: CompoundCluster = mergeGroups(cc3.ops(0), cc3.children(0), cc3.children(1)) 
    print(group1) 
    testResults = testResults.:+(testIfEqual(group1.toString(), "(mike + 3 - 4 + mike)")) 
    
    
    // (mike + 3 + 4 - mike)
    var group2: CompoundCluster = mergeGroups("+", cc3.children(0), cc3.children(1)) 
    print(group2) 
    testResults = testResults.:+(testIfEqual(group2.toString(), "(mike + 3 + 4 - mike)")) 
    
    // ((mike + 3) * (4 - mike))
    var group3: CompoundCluster = mergeGroups("*", cc3.children(0), cc3.children(1)) 
    print(group3) 
    testResults = testResults.:+(testIfEqual(group3.toString(), "((mike + 3) * (4 - mike))")) 
    
    // ((mike + 3) / (4 - mike))
    var group4: CompoundCluster = mergeGroups("/", cc3.children(0), cc3.children(1)) 
    print(group4) 
    testResults = testResults.:+(testIfEqual(group4.toString(), "((mike + 3) / (4 - mike))")) 
    
    // (55 + mike + 3 - 4 + mike)
    var group5: CompoundCluster = mergeGroups(cc4.ops(0), cc4.children(0), cc4.children(1)) 
    print(group5) 
    testResults = testResults.:+(testIfEqual(group5.toString(), "(55 + mike + 3 - 4 + mike)")) 
    
    // (mike - mike + mike - mike)
    var group6: CompoundCluster = mergeGroups(cc5.ops(0), cc5.children(0), cc5.children(1)) 
    print(group6) 
    testResults = testResults.:+(testIfEqual(group6.toString(), "(mike - mike + mike - mike)")) 
    
    // ((mike + 3 - 4 + mike) * (55 + mike + 3 - 4 + mike))
    var group7: CompoundCluster = mergeGroups(cc6.ops(0), cc6.children(0), cc6.children(1)) 
    print(group7) 
    testResults = testResults.:+(testIfEqual(group7.toString(), "((mike + 3 - 4 + mike) * (55 + mike + 3 - 4 + mike))")) 
    
    // ((mike + 3 + 4 - mike) * (55 + mike + 3 - 4 + mike))
    var group8: CompoundCluster = mergeGroups(cc7.ops(0), cc7.children(0), cc7.children(1)) 
    print(group8) 
    testResults = testResults.:+(testIfEqual(group8.toString(), "((mike + 3 + 4 - mike) * (55 + mike + 3 - 4 + mike))")) 
    
    // ((mike + 1 - mike) ^ 3)
    var group9: CompoundCluster = mergeGroups(cc8.ops(0), cc8.children(0), cc8.children(1)) 
    print(group9) 
    testResults = testResults.:+(testIfEqual(group9.toString(), "((mike + 1 - mike) ^ 3)")) 
    
    // ((mike + 1 - mike) ^ james)
    var group10: CompoundCluster = mergeGroups(cc9.ops(0), cc9.children(0), cc9.children(1)) 
    print(group10) 
    testResults = testResults.:+(testIfEqual(group10.toString(), "((mike + 1 - mike) ^ james)")) 
    
    // (mike + james + taylorSwift + selenaGomez)
    var group11: CompoundCluster = mergeGroups(cc10.ops(0), cc10.children(0), cc10.children(1)) 
    print(group11) 
    testResults = testResults.:+(testIfEqual(group11.toString(), "(mike + james + taylorSwift + selenaGomez)")) 
    
    for (element <- testResults) {
      if (!element) {
        println("\n\t--------------------------------------") 
        println("\t- Merge tests had a FAILURE") 
        println("\t--------------------------------------") 
        return 
      }
    }
    
    var noBinding:Map[Symbol, Value] = Map()
    
    println("\nTESTING FINAL SIMPLIFY TESTS: ") 
    
    // Should be: (mike + -1 + mike)
    print(simplifyCompound(compound3, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(compound3, noBinding).toString(), "(mike + -1 + mike)")) 
    print("vprint version (not tested): ") 
    vprint(compound3) 
    println 
    
    // Should be: (54 + mike + mike)
    print(simplifyCompound(compound5, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(compound5, noBinding).toString(), "(54 + mike + mike)")) 
    print("vprint version (not tested): ") 
    vprint(compound5) 
    println 
    
    // Should be: 0
    print(simplifyCompound(longCompoundTest, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(longCompoundTest, noBinding).toString(), "0")) 
    print("vprint version (not tested): ") 
    vprint(longCompoundTest) 
    println 
    
    // Should be: ((mike + -1 + mike) * (54 + mike + mike))
    print(simplifyCompound(test4, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(test4, noBinding).toString(), "((mike + -1 + mike) * (54 + mike + mike))")) 
    print("vprint version (not tested): ") 
    vprint(test4) 
    println 
    
    // Should be: (7 * (54 + mike + mike))
    print(simplifyCompound(test5, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(test5, noBinding).toString(), "(7 * (54 + mike + mike))")) 
    print("vprint version (not tested): ") 
    vprint(test5) 
    println 
    
    // Should be: 1
    print(simplifyCompound(test6, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(test6, noBinding).toString(), "1")) 
    print("vprint version (not tested): ") 
    vprint(test6) 
    println 
    
    // Should be: (1 ^ james)
    print(simplifyCompound(test7, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(test7, noBinding).toString(), "(1 ^ james)")) 
    print("vprint version (not tested): ") 
    vprint(test7) 
    println 
    
    // Should be: (1 ^ james)
    print(simplifyCompound(test2, noBinding)) 
    testResults = testResults.:+(testIfEqual(simplifyCompound(test2, noBinding).toString(), "(mike + james + taylorSwift + selenaGomez)")) 
    print("vprint version (not tested): ") 
    vprint(test2) 
    println 
    
    for (element <- testResults) {
      if (!element) {
        println("\n\t--------------------------------------") 
        println("\t- Final simplify tests had a FAILURE") 
        println("\t--------------------------------------") 
        return 
      }
    }
    
    // Else, all tests passed.
    println("\n\t+++++++++++++++++++++++++++++++++++++") 
    println("\t+ ALL TESTS PASSED") 
    println("\t+++++++++++++++++++++++++++++++++++++") 
  }
  
}