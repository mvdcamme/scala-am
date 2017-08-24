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

import scala.collection.mutable.{Map => MMap}

import com.microsoft.z3._

object Z3 {

  class TestFailedException extends Exception("Check FAILED") {
  }

  var debug: Boolean = false

  private def atomToExpr(atom: ConcolicAtom, ctx: Context, exprMap: MMap[String, IntExpr]): IntExpr = atom match {
    case ConcolicInt(i) =>
      ctx.mkInt(i)
    case i @ ConcolicInput(_) =>
      ctx.mkIntConst(i.toString)
    case ConcolicVariable(symVar, _) =>
      exprMap(symVar)
  }

  private def handleExpression(
    exp: ConcolicExpression,
    ctx: Context,
    exprMap: MMap[String, IntExpr],
    solver: Solver): Unit = exp match {
    case i@ConcolicInt(_) =>
      exprMap.put(i.toString, ctx.mkInt(i.i))
    case i@ConcolicInput(_) =>
      exprMap.put(i.toString, ctx.mkIntConst(i.toString))
    case s@ConcolicVariable(symVar, _) =>
      exprMap.put(s.toString, ctx.mkIntConst(symVar))
    case BinaryConcolicExpression(lhs, op, rhs) => op match {
      case "<" =>
        val lhsExpr = atomToExpr(lhs, ctx, exprMap)
        val rhsExpr = atomToExpr(rhs, ctx, exprMap)
        val condition = ctx.mkLt(lhsExpr, rhsExpr)
        solver.assert_(condition)
    }
  }

  private def getLhs(constraint: ConcolicConstraint): ConcolicExpression = constraint match {
    case BranchConstraint(exp, _) => exp match {
      case BinaryConcolicExpression(lhs, _, _) =>
        lhs
      case _ =>
        exp
    }
    case StatementConstraint(symVar, _, originalVar) =>
      ConcolicVariable(symVar, originalVar)
  }
  private def getLhs(exp: ConcolicExpression): ConcolicExpression = exp match {
    case BinaryConcolicExpression(lhs, _, _) =>
      lhs
    case _ =>
      exp
  }
  private def getRhs(constraint: ConcolicConstraint): Option[ConcolicExpression] = constraint match {
    case BranchConstraint(exp, _) => exp match {
      case BinaryConcolicExpression(_, _, rhs) =>
        Some(rhs)
      case _ =>
        None
    }
    case StatementConstraint(_, exp, _) =>
      Some(exp)
  }
  private def getRhs(exp: ConcolicExpression): Option[ConcolicExpression] = exp match {
    case BinaryConcolicExpression(_, _, rhs) =>
      Some(rhs)
    case _ =>
      None
  }

  // TODO Move to ConcolicExpression class
  private def isSomeVar(exp: ConcolicExpression): Boolean = exp match {
    case ConcolicVariable(_, _) | ConcolicInput(_) =>
      true
    case _ =>
      false
  }

  // TODO Move to ConcolicExpression class
  private def isSomeInt(exp: ConcolicExpression): Boolean = exp match {
    case ConcolicInt(_) =>
      true
    case _ =>
      false
  }

  private def maybeAddExpToMap(exp: ConcolicExpression, ctx: Context, exprMap: MMap[String, IntExpr]): Unit = {
    val expString = exp.toString
    if (isSomeVar(exp)) {
      // exp is a ConcolicVariable or a ConcolicInput
      exprMap.put(expString, ctx.mkIntConst(expString))
    } else if (isSomeInt(exp)) {
      exprMap.put(expString, ctx.mkInt(expString.toInt))
    }
  }

  def constraintSolver(ctx: Context, conslist: List[ConcolicConstraint]): MMap[String, Int] = {
    val exprMap: MMap[String, IntExpr] = scala.collection.mutable.HashMap[String, IntExpr]()
    val solver: Solver = ctx.mkSolver
    println(s"Solving constraints $conslist")
    for (constraint <- conslist) {
      // Basically assumes that lhs is ALWAYS a symbolic variable: predicates such as (0 < x) are not permitted
      val lhs = getLhs(constraint)
      exprMap.put(lhs.toString, ctx.mkIntConst(lhs.toString))
      val optC = getRhs(constraint)
      optC match {
        case Some(c) =>
          val cString = c.toString
          val rhs = getLhs(c)
          val rhsString = rhs.toString
          maybeAddExpToMap(rhs, ctx, exprMap)

          constraint match {
            case StatementConstraint(symVar, exp, _) => exp match {
              case ConcolicInt(i) =>
                // cString should actually be the same as just i.toString, so ctx.mkInt(i) should equal exprMap(cString)
                solver.assert_(ctx.mkEq(exprMap(lhs.toString), ctx.mkInt(i)))
              case ConcolicInput(_) =>
                solver.assert_(ctx.mkEq(exprMap(lhs.toString), exprMap(cString)))
              case ConcolicVariable(_, _) =>
                solver.assert_(ctx.mkEq(exprMap(lhs.toString), exprMap(cString)))
              case BinaryConcolicExpression(lhsExp, op, rhsExp) =>
                // if constraint is a StatementConstraint with a BinaryConcolicExpression, rhs of constraint should equal lhs of binary expression
                assert(rhs == lhsExp)
                maybeAddExpToMap(rhsExp, ctx, exprMap)
                op match {
                  case "+" =>
                    solver.assert_(ctx.mkEq(exprMap(lhs.toString), ctx.mkAdd(exprMap(lhsExp.toString), exprMap(rhsExp.toString))))
                  case "-" =>
                    solver.assert_(ctx.mkEq(exprMap(lhs.toString), ctx.mkSub(exprMap(lhsExp.toString), exprMap(rhsExp.toString))))
                  case "*" =>
                    solver.assert_(ctx.mkEq(exprMap(lhs.toString), ctx.mkMul(exprMap(lhsExp.toString), exprMap(rhsExp.toString))))
                  case "/" =>
                    solver.assert_(ctx.mkEq(exprMap(lhs.toString), ctx.mkDiv(exprMap(lhsExp.toString), exprMap(rhsExp.toString))))
              }
            }
            case BranchConstraint(exp, _) => exp match {
              case i @ ConcolicInt(_) =>
              case i @ ConcolicInput(_) =>
              case s @ ConcolicVariable(symVar, _) =>
              case BinaryConcolicExpression(_, op, _) => op match {
                case "<" =>
                    solver.assert_(ctx.mkLt(exprMap(lhs.toString), exprMap(cString)))
                case "<=" =>
                  solver.assert_(ctx.mkLe(exprMap(lhs.toString), exprMap(cString)))
                case ">" =>
                  solver.assert_(ctx.mkGt(exprMap(lhs.toString), exprMap(cString)))
                case ">=" =>
                  solver.assert_(ctx.mkGe(exprMap(lhs.toString), exprMap(cString)))
                case "=" =>
                  solver.assert_(ctx.mkEq(exprMap(lhs.toString), exprMap(cString)))
              }
            }
          }

        case None =>
      }
    }
      if (Status.SATISFIABLE eq solver.check) {
        val model: Model = solver.getModel
        val nConsts = model.getNumConsts
        val consts = model.getConstDecls
        val decls = model.getDecls
        val funDecls = model.getFuncDecls
        val result = MMap[String, Int]()
        exprMap.foreach({
          case (someString, exp) =>
            if (someString.contains("i")) {
              println(s"$someString should be ${model.getConstInterp(exp)}")
              result.put(someString, model.getConstInterp(exp).toString.toInt)
            }
        })
        println(s"Works: exprMap = $exprMap")
        result
      } else {
        println("Uh oh...")
        ???
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

  def solve(constraints: List[ConcolicConstraint]): Option[MMap[String, Int]] = {
    try {
      val cfg: java.util.HashMap[String, String] = new java.util.HashMap[String, String]
      cfg.put("model", "true")
      val ctx: Context = new Context(cfg)
      if (constraints.nonEmpty) {
        Some(constraintSolver(ctx, constraints))
      } else {
        None
      }
    } catch {
      case ex: Z3Exception =>
        System.out.println("TEST CASE FAILED: " + ex.getMessage)
        System.out.println("Stack trace: ")
        ex.printStackTrace(System.out)
        None
      case ex: TestFailedException =>
        System.out.println(s"Error solving constraints $constraints")
        None
      case ex: Exception =>
        System.out.println("Unknown Exception: " + ex.getMessage)
        None
    }
  }
}
