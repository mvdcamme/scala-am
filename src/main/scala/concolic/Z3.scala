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

  private def expToExpr(exp: ConcolicExpression, ctx: Context, exprMap: MMap[ConcolicInput, IntExpr]): ArithExpr = exp match {
    case ConcolicInt(i) => ctx.mkInt(i)
    case i: ConcolicInput => exprMap.get(i) match {
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
          case "!=" =>
            solver.assert_(ctx.mkNot(ctx.mkEq(lExpr, rExpr)))
        }
    }
  }

  def constraintSolver(ctx: Context, conslist: List[BranchConstraint]): Z3Result = {
    val exprMap: MMap[ConcolicInput, IntExpr] = scala.collection.mutable.HashMap[ConcolicInput, IntExpr]()
    val solver: Solver = ctx.mkSolver
    Logger.log(s"Z3 solving constraints $conslist", Logger.V)
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
            Logger.log(s"$someInput should be ${model.getConstInterp(exp)}", Logger.U)
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
      Logger.log(s"Z3, in solve, constraints are $constraints", Logger.V)
      if (constraints.nonEmpty) {
        constraintSolver(ctx, constraints)
      } else {
        SomeZ3Error
      }
    } catch {
      case ex: Z3Exception =>
        Logger.log("TEST CASE FAILED: " + ex.getMessage, Logger.U)
        Logger.log("Stack trace: ", Logger.U)
        ex.printStackTrace(System.out)
        SomeZ3Error
      case ex: TestFailedException =>
        Logger.log(s"Error solving constraints $constraints", Logger.U)
        SomeZ3Error
      case ex: Exception =>
        Logger.log("Unknown Exception: " + ex.getMessage, Logger.U)
        SomeZ3Error
    }
  }
}
