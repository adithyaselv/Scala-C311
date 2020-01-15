package com.plp.natural_recursion

object week1 extends App {

  /** Implementing Plus using natural recursion
    *
    * @param n arg1 Todo: Make the type restricted to only Natural Numbers ?
    * @param m arg2 Todo: Make the type restricted to only Natural Numbers ?
    * @return
    */
  def plus(n: Int , m: Int): Int = {
    n match {
      case 0 => m
      case _ => 1 + plus(n - 1, m)
    }
  }

  println(plus(3,5))

  /** Implementing Multiply using natural recursion
    *
    * @param n arg1 Todo: Make the type restricted to only Natural Numbers ?
    * @param m arg2 Todo: Make the type restricted to only Natural Numbers ?
    * @return
    */
  def multiply(n: Int , m: Int): Int = {
    n match {
      case 0 => 0
      case _ => m + multiply(n - 1, m)
    }
  }

  println(multiply(3,5))

  /** Implementing power using natural recursion
    *
    * @param n arg1 Todo: Make the type restricted to only Natural Numbers ?
    * @param m arg2 Todo: Make the type restricted to only Natural Numbers ?
    * @return
    */
  def power(n: Int , m: Int): Int = {
    m match {
      case 0 => 1
      case _ => n * power(n, m-1)
    }
  }

  println(power(3,5))

  def countOccurs[A](x: A, ls: List[A]): Int = {
    ls match {
      case Nil => 0
      case head :: tail if head == x => 1 + countOccurs(x, tail)
      case _ :: tail => countOccurs(x, tail)
    }
  }

  println(countOccurs(3, List(1, 3, 4, 3, 5, 7)))
  println(countOccurs('a', List('b', 'b', 'a', 'a', 'a', 'a')))

  def member[A](x: A, ls: List[A]): Boolean = {
    ls match {
      case Nil => false
      case head :: tail => head == x || member(x, tail)
    }
  }

  println(member("cat", List("cat", "dog", "dog")))

}