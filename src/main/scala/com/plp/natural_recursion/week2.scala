package com.plp.natural_recursion

object week2 extends App {

  val some_sentence = List("Mary","Had", "a", "little", "lamb")
  val some_other_sentence = List("Mary","Had", "a", "little", "lion")

  def f(x: List[String]): List[String] = {
    x match {
      case List(person,"Had", "a", "little", animal) => List(person, "and", animal)
      case _ => List("nothing-matched")
    }
  }

  println(f(some_other_sentence))

  /*** Lambda calculus
    *  a lambda expression is one of the following
    * y                       if y is a variable
    * (lambda (x) body)       if x is a variable and body is a lambda expression
    * (rator rand)            if rator and rand are lambda expressions
    */

    sealed trait Exp
    case class Y(y: String) extends Exp
    case class Lambda(x: Y, body: Exp) extends Exp
    case class App(rator:Exp, rand:Exp) extends Exp

}
