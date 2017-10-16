/**
  * *****************************************************************************
  * Copyright (c) 2016, KAIST.
  * All rights reserved.
  *
  * Use is subject to license terms.
  *
  * This distribution may include materials developed by third parties.
  * ****************************************************************************
  */

import com.microsoft.z3._

import scala.collection.mutable.{Map => MMap}

trait Z3Result

case class Satisfiable(solution: MMap[String, Int]) extends Z3Result
case object Unsatisfiable extends Z3Result
case object SomeZ3Error extends Z3Result



object Z3 {

  class TestFailedException extends Exception("Check FAILED") {
  }

//  private def getLhs(constraint: ConcolicConstraint): ConcolicExpression = constraint match {
//    case BranchConstraint(exp) => exp match {
//      case BinaryConcolicExpression(lhs, _, _) =>
//        lhs
//      case _ =>
//        exp
//    }
//    case StatementConstraint(symVar, _, originalVar) =>
//      ConcolicVariable(symVar, originalVar)
//  }
//  private def getLhs(exp: ConcolicExpression): ConcolicExpression = exp match {
//    case BinaryConcolicExpression(lhs, _, _) =>
//      lhs
//    case _ =>
//      exp
//  }
//  private def getRhs(constraint: ConcolicConstraint): Option[ConcolicExpression] = constraint match {
//    case BranchConstraint(exp) => exp match {
//      case BinaryConcolicExpression(_, _, rhs) =>
//        Some(rhs)
//      case _ =>
//        None
//    }
//    case StatementConstraint(_, exp, _) =>
//      Some(exp)
//  }
//  private def getRhs(exp: ConcolicExpression): Option[ConcolicExpression] = exp match {
//    case BinaryConcolicExpression(_, _, rhs) =>
//      Some(rhs)
//    case _ =>
//      None
//  }

  // TODO Move to ConcolicExpression class
  private def isSomeInt(exp: ConcolicExpression): Boolean = exp match {
    case ConcolicInt(_) =>
      true
    case _ =>
      false
  }

  private def expToExpr(exp: ConcolicExpression, ctx: Context, exprMap: MMap[ConcolicInput, IntExpr]): ArithExpr = exp match {
    case ConcolicInt(i) => ctx.mkInt(i)
    case i @ ConcolicInput(_) => exprMap.get(i) match {
        case Some(expr) => expr
        case None =>
          val newExpr = ctx.mkIntConst(i.toString)
          exprMap.put(i, newExpr)
          newExpr
      }
    case ArithmeticalConcolicExpression(op, exps) =>
      val exprs = exps.map(expToExpr(_, ctx, exprMap))
      val expr = op match {
        case "+" =>
          ctx.mkAdd(exprs: _*)
        case "-" =>
          ctx.mkSub(exprs: _*)
        case "*" =>
          ctx.mkMul(exprs: _*)
        case "/" =>
          assert(exprs.size == 2) // TODO Refactor ArithmeticalConcolicExpression so there are separate classes for variadic expressions and fixed-size expressions
          ctx.mkDiv(exprs.head, exprs(1))
      }
      expr
  }

  private def addConstraint(solver: Solver, ctx: Context,
                            exprMap: MMap[ConcolicInput, IntExpr], concolicExp: RelationalConcolicExpression): Unit = {
    concolicExp match {
      case RelationalConcolicExpression(lhsExp, op, rhsExp) =>
        val lExpr = expToExpr(lhsExp, ctx, exprMap)
        val rExpr = expToExpr(rhsExp, ctx, exprMap)
        op match {
          case "<" =>
            solver.assert_(ctx.mkLt(lExpr, rExpr))
          case "<=" =>
            solver.assert_(ctx.mkLe(lExpr, rExpr))
          case ">" =>
            solver.assert_(ctx.mkGt(lExpr, rExpr))
          case ">=" =>
            solver.assert_(ctx.mkGe(lExpr, rExpr))
          case "=" =>
            solver.assert_(ctx.mkEq(lExpr, rExpr))
        }
    }
  }

  def constraintSolver(ctx: Context, conslist: List[BranchConstraint]): Z3Result = {
    val exprMap: MMap[ConcolicInput, IntExpr] = scala.collection.mutable.HashMap[ConcolicInput, IntExpr]()
    val solver: Solver = ctx.mkSolver
    Logger.log(s"Z3 solving constraints $conslist", Logger.U)
    for (constraint <- conslist) {
        addConstraint(solver, ctx, exprMap, constraint.exp)
    }
      if (Status.SATISFIABLE eq solver.check) {
        val model: Model = solver.getModel
        val nConsts = model.getNumConsts
        val consts = model.getConstDecls
        val decls = model.getDecls
        val funDecls = model.getFuncDecls
        val result = MMap[ConcolicInput, Int]()
        exprMap.foreach({
          case (someInput, exp) =>
              println(s"$someInput should be ${model.getConstInterp(exp)}")
              result.put(someInput, model.getConstInterp(exp).toString.toInt)
        })
        Satisfiable(result.map( { case (key, value) => (key.toString, value) } ))
      } else {
        Unsatisfiable
      }
  }

  def solve(constraints: List[BranchConstraint]): Z3Result = {
    try {
      val cfg: java.util.HashMap[String, String] = new java.util.HashMap[String, String]
      cfg.put("model", "true")
      val ctx: Context = new Context(cfg)
      println(s"Z3, in solve, constraints are $constraints")
      if (constraints.nonEmpty) {
        constraintSolver(ctx, constraints)
      } else {
        SomeZ3Error
      }
    } catch {
      case ex: Z3Exception =>
        System.out.println("TEST CASE FAILED: " + ex.getMessage)
        System.out.println("Stack trace: ")
        ex.printStackTrace(System.out)
        SomeZ3Error
      case ex: TestFailedException =>
        System.out.println(s"Error solving constraints $constraints")
        SomeZ3Error
      case ex: Exception =>
        System.out.println("Unknown Exception: " + ex.getMessage)
        SomeZ3Error
    }
  }
}
