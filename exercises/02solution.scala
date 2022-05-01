/**
Homework 02
============
Note: For some tasks, test examples are already provided.
Be sure to provide tests for all tasks and check your solution with them.
From now on, the tasks will not explicitly require tests any more,
but I advise you to nevertheless use tests for all programming tasks.
Deadline: May 02, 2022, 10:00h  */

/**
Task 1: Visitors (1 subtask)
------
 */

object Hw02Task1 {
  /**
  Consider the definition for the count visitor and the print visitor for AE from the lecture.
   */
  case class Visitor[T](num: Int => T, add: (T, T) => T)

  sealed trait Exp

  case class Num(n: Int) extends Exp
  case class Add(lhs: Exp, rhs: Exp) extends Exp

  // Fold using visitors
  def foldExp[T](v: Visitor[T], e: Exp): T =
    e match {
      case Num(n)    => v.num(n)
      case Add(l, r) => v.add(foldExp(v, l), foldExp(v, r))
    }

  val countVisitor = Visitor[Int]( _=>1, _+_)
  val printVisitor = Visitor[String](_.toString, "("+_+"+"+_+")")

  /**
  Subtasks:
      1) Translate countVisitor and printVisitor to a definition using pattern matching.
      Example: Translating the eval visitor in this way leads to the eval method for object AE from the lecture.
      (https://ps-tuebingen-courses.github.io/pl1-lecture-notes/04-desugaring/desugaring.html)
   */

  /**
  val evalVisitorAE = VisitorAE[Int](x => x, (a, b) => a + b)

  def eval(e: Exp): Int =
    e match {
      case Num(n) => n
      case Add(lhs, rhs) =>
        eval(lhs) + eval(rhs)
    }
   */

  def evalCount(e: Exp): Int =
    e match {
        case Num(n) => 1
        case Add(lhs, rhs) => evalCount(lhs) + evalCount(rhs)
    }

  def evalPrint(e: Exp): String =
    e match {
        case Num(n) => n.toString
        case Add(lhs, rhs) => evalPrint(lhs) + "+" + evalPrint(rhs)
    }

}


/**
Task 2: Desugaring to Nand (1 subtask)
------
 */

import scala.language.implicitConversions

object Hw02Task2 {
  /**
  Consider again the language of propositional logic formulae from the previous homework:
   */
  sealed abstract class Exp
  case class True() extends Exp  // constant true
  case class False() extends Exp // constant false
  case class And(lhs: Exp, rhs: Exp) extends Exp
  case class Or(lhs: Exp, rhs: Exp) extends Exp
  case class Not(e: Exp) extends Exp
  case class Impl(lhs: Exp, rhs: Exp) extends Exp

  def eval(e: Exp) : Boolean = e match {
    case True()     => true
    case False()    => false
    case Nand(lhs, rhs) => if (!eval(lhs) && !eval(rhs)) true else false
    case _          => sys.error("not yet implemented")
  }

  /**
  Subtasks:
      1) Introduce a new kind of expression "Nand" (not both ... and ...).
      Eliminate And, Or, Not, and Impl by defining them as syntactic sugar for Nand.
   */

  case class Nand(lhs: Exp, rhs: Exp) extends Exp

  def and(e1: Exp, e2: Exp) : Exp = not(Nand(e1, e2))
  def not(e:Exp): Exp = Nand(e, e)
  def or(e1: Exp, e2: Exp) : Exp = not(and(not(e1), not(e2)))
  def impl(e1: Exp, e2: Exp) : Exp = or(not(e1), e2)
}

