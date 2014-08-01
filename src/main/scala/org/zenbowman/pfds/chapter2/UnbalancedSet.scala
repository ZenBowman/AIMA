package org.zenbowman.pfds.chapter2

import scala.math.Ordered

trait Set[T] {
  def empty: Set[T]
  def insert(some: T): Set[T]
  def member(some: T): Boolean
}

abstract class UnbalancedSet[T <: Ordered[T]] extends Set[T] {
  def empty: UnbalancedSet[T]
  def insert(some: T): UnbalancedSet[T]
}

case class EmptySet[T <: Ordered[T]]() extends UnbalancedSet[T] {
  override def empty: UnbalancedSet[T] = EmptySet()
  override def insert(some: T) =
    NonEmptySet(EmptySet(), some, EmptySet())
  override def member(some: T) = false
}

case class NonEmptySet[T <: Ordered[T]]
  (l: UnbalancedSet[T], v: T, r: UnbalancedSet[T])
    extends UnbalancedSet[T] {

  def empty: UnbalancedSet[T] = EmptySet()
  def insert(some: T): UnbalancedSet[T] = {
    if (some.compare(v) < 0) {
      NonEmptySet(l.insert(some), v, r)
    }
    else if (v < some) {
      NonEmptySet(l, v, r.insert(some))
    }
    else {
      this
    }
  }

  def member(some: T) = {
    if (some < v) l.member(some)
    else if (v < some) r.member(some)
    else true
  }
}


case class V(i: Int) extends Ordered[V] {
  def compare(that: V) = i - that.i
}


