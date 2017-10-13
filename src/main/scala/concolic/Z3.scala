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

//      if (exp.getOp.isDefined) {
//        val op: String = exp.getOp.get
//        val lhs: String = exp.getLhs.get
//        exprMap.put(lhs, ctx.mkIntConst(lhs))
//        if (exp.getRhs.isDefined) {
//          val c: String = exp.getRhs.get
//          val rhs: String = c.getLhs.getValue
//          if (rhs.contains("s") || rhs.contains("i")) {
//            exprMap.put(rhs, ctx.mkIntConst(rhs))
//          } else {
//            exprMap.put(rhs, ctx.mkInt(rhs.toInt))
//          }
//          op.charAt(0) match {
//            case '=' =>
//              if (op.length > 1 && op.charAt(1) == '=') solver.assert_(ctx.mkEq(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              else {
//                if (c.getOp.isDefined) {
//                  if (c.getRhs.isDefined) {
//                    val v: String = c.getRhs.get.getLhs.getValue
//                    if (v.contains("s") || v.contains("i") || v.contains("this")) exprMap.put(v, ctx.mkIntConst(v))
//                    else exprMap.put(v, ctx.mkInt(v.toInt))
//                    val constraint_op: String = c.getOp.get
//                    constraint_op.charAt(0) match {
//                      case '+' =>
//                        solver.assert_(ctx.mkEq(exprMap.get(lhs).get, ctx.mkAdd(exprMap.get(rhs).get, exprMap.get(v).get)))
//                      case '-' =>
//                        solver.assert_(ctx.mkEq(exprMap.get(lhs).get, ctx.mkSub(exprMap.get(rhs).get, exprMap.get(v).get)))
//                      case '*' =>
//                        solver.assert_(ctx.mkEq(exprMap.get(lhs).get, ctx.mkMul(exprMap.get(rhs).get, exprMap.get(v).get)))
//                      case '/' =>
//                        solver.assert_(ctx.mkEq(exprMap.get(lhs).get, ctx.mkDiv(exprMap.get(rhs).get, exprMap.get(v).get)))
//                      case '%' =>
//                        solver.assert_(ctx.mkEq(exprMap.get(lhs).get, ctx.mkMod(exprMap.get(rhs).get, exprMap.get(v).get)))
//                      case '&' =>
//                        val x: BitVecExpr = ctx.mkInt2BV(1, exprMap.get(rhs).get)
//                        val y: BitVecExpr = ctx.mkInt2BV(1, exprMap.get(v).get)
//                        solver.assert_(ctx.mkEq(exprMap.get(lhs).get, ctx.mkBV2Int(ctx.mkBVAND(x, y), false)))
//                      case '!' =>
//                        solver.assert_(ctx.mkITE(ctx.mkDistinct(exprMap.get(rhs).get, exprMap.get(v).get), ctx.mkDistinct(exprMap.get(lhs).get, ctx.mkInt(0)), ctx.mkEq(exprMap.get(lhs).get, ctx.mkInt(0))).asInstanceOf[BoolExpr])
//                      case '=' =>
//                        solver.assert_(ctx.mkITE(ctx.mkEq(exprMap.get(rhs).get, exprMap.get(v).get), ctx.mkDistinct(exprMap.get(lhs).get, ctx.mkInt(0)), ctx.mkEq(exprMap.get(lhs).get, ctx.mkInt(0))).asInstanceOf[BoolExpr])
//                      case '>' =>
//                        val condition: BoolExpr =
//                          if (constraint_op.length > 1 && constraint_op.charAt(1) == '=')
//                            ctx.mkGe(exprMap.get(rhs).get, exprMap.get(v).get)
//                          else
//                            ctx.mkGt(exprMap.get(rhs).get, exprMap.get(v).get)
//                        solver.assert_(ctx.mkITE(condition, ctx.mkDistinct(exprMap.get(lhs).get, ctx.mkInt(0)), ctx.mkEq(exprMap.get(lhs).get, ctx.mkInt(0))).asInstanceOf[BoolExpr])
//                      case '<' =>
//                        val condition =
//                          if (constraint_op.length > 1 && constraint_op.charAt(1) == '=')
//                            ctx.mkLe(exprMap.get(rhs).get, exprMap.get(v).get)
//                          else
//                            ctx.mkLt(exprMap.get(rhs).get, exprMap.get(v).get)
//                        solver.assert_(ctx.mkITE(condition, ctx.mkDistinct(exprMap.get(lhs).get, ctx.mkInt(0)), ctx.mkEq(exprMap.get(lhs).get, ctx.mkInt(0))).asInstanceOf[BoolExpr])
//                      case _ =>
//                        System.out.println("Not yet supported")
//                        throw new TestFailedException
//                    }
//                  } else {
//                    System.out.println("Wrong exp form" + c)
//                    throw new TestFailedException
//                  }
//                } else solver.assert_(ctx.mkEq(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              }
//            case '<' =>
//              if (op.length > 1 && op.charAt(1) == '=') {
//                solver.assert_(ctx.mkLe(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              } else {
//                solver.assert_(ctx.mkLt(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              }
//            case '>' =>
//              if (op.length > 1 && op.charAt(1) == '=') {
//                solver.assert_(ctx.mkGe(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              } else {
//                solver.assert_(ctx.mkGt(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              }
//            case '!' =>
//              if (op.length > 1 && op.charAt(1) == '=') {
//                solver.assert_(ctx.mkDistinct(exprMap.get(lhs).get, exprMap.get(rhs).get))
//              } else {
//                System.out.println("Wrong exp form" + op)
//                throw new TestFailedException
//              }
//            case _ =>
//              System.out.println("Not yet supported")
//              throw new TestFailedException
//          }
//        } else {
//          System.out.println("Wrong exp form" + exp)
//          throw new TestFailedException
//        }
//      } else {
//        System.out.println("Wrong exp form" + exp)
//        throw new TestFailedException
//      }
//    }
//    if (Status.SATISFIABLE eq solver.check) {
//      val model: Model = solver.getModel
//      if (debug) {
//        System.out.println("Solver = " + solver)
//        System.out.println("Model = " + model)
//      }
//      val result: HashMap[String, Integer] = new HashMap[String, Integer]
//      if (exprMap.contains("this")) {
//        result.put("this", model.getConstInterp(exprMap.get("this").get).toString.toInt)
//      }
//      var i: Int = 0
//      while (i < inum) {
//        if (exprMap.contains("i" + i)) {
//          result.put("i" + i, model.getConstInterp(exprMap.get("i" + i).get).toString.toInt)
//        }
//        i += 1
//      }
//      result
//    } else {
//      System.out.println(solver.check)
//      throw new TestFailedException
//    }
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
