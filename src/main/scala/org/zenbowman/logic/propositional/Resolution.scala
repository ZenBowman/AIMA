package org.zenbowman.logic.propositional

import scala.collection.mutable

object Resolution {

  import org.zenbowman.logic.propositional.PropositionalLogic._

  def unitResolve(s: ExpandedDisjunction, m: Literal): ExpandedDisjunction = {
    val elems = new mutable.HashSet[Sentence]
    for {
      clause <- s.clauses if clause.isInstanceOf[Literal]
      l = clause.asInstanceOf[Literal]
    } {
      if (l.opposite != m) {
        elems.add(l)
      }
    }
    ExpandedDisjunction(elems.toSet)
  }

  def getComplimentaryLiteral(s1: ExpandedDisjunction, s2: ExpandedDisjunction): Option[Literal] = {
    for (elem1 <- s1.clauses) {
      for (elem2 <- s2.clauses) {
        if (elem1.asInstanceOf[Literal].opposite == elem2.asInstanceOf[Literal]) {
          return Some(elem1.asInstanceOf[Literal])
        }
      }
    }
    None
  }

  def resolve(s1: ExpandedDisjunction, s2: ExpandedDisjunction): Seq[ExpandedDisjunction] = {
    println("Resolving %s with %s".format(s1, s2))
    for (c <- getComplimentaryLiteral(s1, s2)) {
      val allPossibleClauses = new mutable.HashSet[Sentence]()
      val allClauses = new mutable.HashSet[Sentence]

      s1.clauses.foreach(x => allPossibleClauses.add(x))
      s2.clauses.foreach(x => allPossibleClauses.add(x))

      for {
        elem <- allPossibleClauses
        l = elem.asInstanceOf[Literal]
      } {
        if (!((l == c) || (l == c.opposite))) {
          allClauses.add(l)
        }
      }

      if (allClauses.size == 0) {
        throw new EmptyClauseException(s1, s2)
      }

      println("Result = %s".format(ExpandedDisjunction(allClauses.toSet)))
      return List(ExpandedDisjunction(allClauses.toSet))
    }

    List()

  }

  def testResolution(s: Set[ExpandedDisjunction], e: ExpandedDisjunction): TruthValue = {
    try {
      val newElements = resolution(s.union(Set(e)))
      if (newElements.subsetOf(s)) {
        return False
      }
    } catch {
      case e: EmptyClauseException =>
        println(e.toString)
        return True
    }
    Unknown
  }

  def resolution(s: Set[ExpandedDisjunction]): Set[ExpandedDisjunction] = {
    val newSet = new mutable.HashSet[ExpandedDisjunction]()
    for (c_i <- s) {
      for (c_j <- s if c_j != c_i) {
        val resolvents = resolve(c_i, c_j)
        for (resolvent <- resolvents) {
          newSet.add(resolvent)
        }
      }
    }
    newSet.toSet
  }
}
