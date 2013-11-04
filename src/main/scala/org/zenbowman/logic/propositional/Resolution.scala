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
    for (c <- getComplimentaryLiteral(s1, s2)) {
      val allClauses = new mutable.HashSet[Sentence]

      for (elem <- s1.clauses if elem != c) {
        allClauses.add(elem)
      }

      for (elem <- s2.clauses if elem != c.opposite) {
        allClauses.add(elem)
      }

      if (allClauses.size == 0) {
        throw new EmptyClauseException(s1, s2)
      }

      if (allClauses.size == 2) {
        val elem1 = allClauses.head.asInstanceOf[Literal]
        val elem2 = allClauses.tail.head
        if (elem1.opposite == elem2) {
          return List()
        }
      }

      val newClause = ExpandedDisjunction(allClauses.toSet)
      println("Resolved %s and %s to: %s".format(s1, s2, newClause))
      return List(newClause)
    }

    List()

  }

  def testResolution(s: Set[ExpandedDisjunction], e: ExpandedDisjunction): TruthValue = {
    try {
      val newElements = resolution(s.union(Set(e)))
    } catch {
      case e: EmptyClauseException =>
        println(e.toString)
        return True
    }
    Unknown
  }

  def resolution(s: Set[ExpandedDisjunction]): Set[ExpandedDisjunction] = {
    val all = new mutable.HashSet[ExpandedDisjunction]()
    var newSet: Set[ExpandedDisjunction] = Set()
    while(true) {
      newSet = resolutionOnce(s.union(all))
      if (newSet.subsetOf(all)) {
        return all.toSet
      } else {
        all ++= newSet
      }
    }
    Set()
  }

  def resolutionOnce(s: Set[ExpandedDisjunction]): Set[ExpandedDisjunction] = {
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
