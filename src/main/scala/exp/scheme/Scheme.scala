/**
  * Abstract syntax of Scheme programs (probably far from complete)
  */
import scala.util.parsing.input.Position

trait SchemeExp {
  val pos: Position
}

case class SchemePopSymEnv(pos: Position) extends SchemeExp {
  override def toString = "SchemePopSymEnv"
}

/**
  * Bindings must be of the form ([(varname initExp [stepExp])] ...)
  * Test must be of the form: (exp [exp ...])
  * Commands must be of the form [exp ...]
  *
  * Example of a valid do-expression:
  *
  * (do ((a 1 (+ a 1)
  *      (b 1 (+ b 2))
  *     ((= a 10) 'stop)
  *     (displayln a) (displayln b))
  *
  * Iteration then stops when a becomes 10
  *
  * Also valid:
  *
  * (do ()
  *     (#t))
  *
  * Also valid:
  *
  * (do ((a 1))
  *     ((= a 10))
  *     (set! a (+ a 1)))
  */
case class SchemeDo(bindings: List[(String, SchemeExp, SchemeExp)],
                    test: (SchemeExp, List[SchemeExp]),
                    commands: List[SchemeExp],
                    pos: Position)
    extends SchemeExp {
  override def toString(): String = {
    val bi =
      bindings
        .map({ case (name, init, step) => s"($name $init $step)" })
        .mkString(" ")
    val te = s"(${test._1} ${test._2.mkString(" ")})"
    val co = commands.mkString(" ")
    s"(do ($bi) $te $co)"
  }
}

/**
  * An amb expression: (amb exp1 ...).
  * Used by the non-deterministic, ambiguous, Scheme interpreter. See SICP chapter 4.3.
  */
case class SchemeAmb(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val expsString = exps.mkString(" ")
    s"(amb $expsString)"
  }
}

/**
  * A lambda expression: (lambda (args...) body...)
  * Not supported: "rest"-arguments, of the form (lambda arg body), or (lambda (arg1 . args) body...)
  */
case class SchemeLambda(args: List[String],
                        body: List[SchemeExp],
                        pos: Position)
    extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(lambda ($a) $b)"
  }
}

/**
  * A function call: (f args...)
  */
case class SchemeFuncall(f: SchemeExp, args: List[SchemeExp], pos: Position)
    extends SchemeExp {
  override def toString() = {
    if (args.isEmpty) {
      s"($f)"
    } else {
      val a = args.mkString(" ")
      s"($f $a)"
    }
  }
}

/**
  * An if statement: (if cond cons alt)
  * If without alt clauses need to be encoded with an empty begin as alt clause
  */
case class SchemeIf(cond: SchemeExp,
                    cons: SchemeExp,
                    alt: SchemeExp,
                    pos: Position)
    extends SchemeExp {
  override def toString() = s"(if $cond $cons $alt)"
}

/**
  * Let-bindings: (let ((v1 e1) ...) body...)
  */
case class SchemeLet(bindings: List[(String, SchemeExp)],
                     body: List[SchemeExp],
                     pos: Position)
    extends SchemeExp {
  override def toString() = {
    val bi =
      bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let ($bi) $bo)"
  }
}

/**
  * Let*-bindings: (let* ((v1 e1) ...) body...)
  */
case class SchemeLetStar(bindings: List[(String, SchemeExp)],
                         body: List[SchemeExp],
                         pos: Position)
    extends SchemeExp {
  override def toString() = {
    val bi =
      bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(let* ($bi) $bo)"
  }
}

/**
  * Letrec-bindings: (letrec ((v1 e1) ...) body...)
  */
case class SchemeLetrec(bindings: List[(String, SchemeExp)],
                        body: List[SchemeExp],
                        pos: Position)
    extends SchemeExp {
  override def toString() = {
    val bi =
      bindings.map({ case (name, exp) => s"($name $exp)" }).mkString(" ")
    val bo = body.mkString(" ")
    s"(letrec ($bi) $bo)"
  }
}

/**
  * A set! expression: (set! variable value)
  */
case class SchemeSet(variable: String, value: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString() = s"(set! $variable $value)"
}

/**
  * A begin clause: (begin body...)
  */
case class SchemeBegin(exps: List[SchemeExp], pos: Position)
    extends SchemeExp {
  override def toString() = {
    val body = exps.mkString(" ")
    s"(begin $body)"
  }
}

/**
  * A cond expression: (cond (test1 body1...) ...)
  */
case class SchemeCond(clauses: List[(SchemeExp, List[SchemeExp])],
                      pos: Position)
    extends SchemeExp {
  override def toString() = {
    val c = clauses
      .map({
        case (cond, cons) => {
          val b = cons.mkString(" ")
          s"($cond $b)"
        }
      })
      .mkString(" ")
    s"(cond $c)"
  }
}

/**
  * A case expression: (case key ((vals1...) body1...) ... (else default...))
  */
case class SchemeCase(key: SchemeExp,
                      clauses: List[(List[SchemeValue], List[SchemeExp])],
                      default: List[SchemeExp],
                      pos: Position)
    extends SchemeExp {
  override def toString() = {
    val c = clauses
      .map({
        case (datums, cons) => {
          val d = datums.mkString(" ")
          val b = cons.mkString(" ")
          s"(($d) $b)"
        }
      })
      .mkString(" ")
    if (default.isEmpty) {
      s"(case $key $c)"
    } else {
      s"(case $key $c (else ${default.mkString(" ")}))"
    }
  }
}

/**
  * An and expression: (and exps...)
  */
case class SchemeAnd(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val e = exps.mkString(" ")
    s"(and $e)"
  }
}

/**
  * An or expression: (or exps...)
  */
case class SchemeOr(exps: List[SchemeExp], pos: Position) extends SchemeExp {
  override def toString() = {
    val e = exps.mkString(" ")
    s"(or $e)"
  }
}

/**
  * A variable definition: (define name value)
  */
case class SchemeDefineVariable(name: String, value: SchemeExp, pos: Position)
    extends SchemeExp {
  override def toString() = s"(define $name $value)"
}

/**
  * A function definition: (define (name args...) body...)
  */
case class SchemeDefineFunction(name: String,
                                args: List[String],
                                body: List[SchemeExp],
                                pos: Position)
    extends SchemeExp {
  override def toString() = {
    val a = args.mkString(" ")
    val b = body.mkString(" ")
    s"(define ($name $a) $b)"
  }
}

/**
  * An identifier: name
  */
case class SchemeIdentifier(name: String, pos: Position) extends SchemeExp {
  override def toString() = name
}

/**
  * A quoted expression: '(foo (bar baz))
  *  The quoted expression is *not* converted to a Scheme expression, and remains
  * a simple s-expression, because that's exactly what it should be.
  */
case class SchemeQuoted(quoted: SExp, pos: Position) extends SchemeExp {
  override def toString() = s"'$quoted"
}

/**
  * A literal value (number, symbol, string, ...)
  */
case class SchemeValue(value: Value, pos: Position) extends SchemeExp {
  override def toString() = value.toString
}

/**
  * Compare-and-swap, concurrency synchronization primitive.
  */
case class SchemeCas(variable: String,
                     eold: SchemeExp,
                     enew: SchemeExp,
                     pos: Position)
    extends SchemeExp {
  override def toString() = s"(cas $variable $eold $enew)"
}

/**
  * Compare-and-swap on a vector
  */
case class SchemeCasVector(variable: String,
                           index: SchemeExp,
                           eold: SchemeExp,
                           enew: SchemeExp,
                           pos: Position)
    extends SchemeExp {
  override def toString() = s"(cas-vector $variable $index $eold $enew)"
}

/**
  * Acquire a lock
  */
case class SchemeAcquire(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(acquire $exp)"
}

/**
  * Release a lock
  */
case class SchemeRelease(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(release $exp)"
}

/**
  * Spawn a new thread to compute an expression
  */
case class SchemeSpawn(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(spawn $exp)"
}

/**
  * Wait for a thread (whose identifier is the value of exp) to terminate
  */
case class SchemeJoin(exp: SchemeExp, pos: Position) extends SchemeExp {
  override def toString() = s"(join $exp)"
}

/**
  * Trait that provides a method to compile an s-expression into a Scheme expression
  */
trait SchemeCompiler {

  /**
    * Reserved keywords
    */
  val reserved: List[String] = List("do",
                                    "amb",
                                    "lambda",
                                    "if",
                                    "let",
                                    "let*",
                                    "letrec",
                                    "cond",
                                    "case",
                                    "set!",
                                    "begin",
                                    "define",
                                    "cas",
                                    "acquire",
                                    "release",
                                    "cas-vector")

  def compile(exp: SExp): SchemeExp = exp match {
    case SExpPair(
        SExpIdentifier("do", _),
        SExpPair(bindings, SExpPair(cond, commands, _), _),
        _) =>
      SchemeDo(compileDoBindings(bindings),
               compileDoCond(cond),
               compileBody(commands),
               exp.pos)
    case SExpPair(SExpIdentifier("amb", _), body, _) =>
      SchemeAmb(compileBody(body), exp.pos)
    case SExpPair(SExpIdentifier("quote", _),
                  SExpPair(quoted, SExpValue(ValueNil, _), _),
                  _) =>
      compile(SExpQuoted(quoted, exp.pos))
    case SExpPair(SExpIdentifier("quote", _), _, _) =>
      throw new Exception(s"Invalid Scheme quote: $exp (${exp.pos})")
    case SExpPair(SExpIdentifier("lambda", _),
                  SExpPair(args, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLambda(compileArgs(args),
                   compile(first) :: compileBody(rest),
                   exp.pos)
    case SExpPair(SExpIdentifier("lambda", _), _, _) =>
      throw new Exception(s"Invalid Scheme lambda: $exp (${exp.pos})")
    case SExpPair(SExpIdentifier("if", _),
                  SExpPair(
                  cond,
                  SExpPair(cons, SExpPair(alt, SExpValue(ValueNil, _), _), _),
                  _),
                  _) =>
      SchemeIf(compile(cond), compile(cons), compile(alt), exp.pos)
    case SExpPair(SExpIdentifier("if", _),
                  SExpPair(cond, SExpPair(cons, SExpValue(ValueNil, _), _), _),
                  _) =>
      /* Empty else branch is replaced by #f (R5RS states it's unspecified) */
      SchemeIf(compile(cond),
               compile(cons),
               SchemeValue(ValueBoolean(false), exp.pos),
               exp.pos)
    case SExpPair(SExpIdentifier("if", _), _, _) =>
      throw new Exception(s"Invalid Scheme if: $exp (${exp.pos})")
    case SExpPair(SExpIdentifier("let", _),
                  SExpPair(bindings, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLet(compileBindings(bindings),
                compile(first) :: compileBody(rest),
                exp.pos)
    case SExpPair(SExpIdentifier("let", _), _, _) =>
      throw new Exception(s"Invalid Scheme let: $exp")
    case SExpPair(SExpIdentifier("let*", _),
                  SExpPair(bindings, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLetStar(compileBindings(bindings),
                    compile(first) :: compileBody(rest),
                    exp.pos)
    case SExpPair(SExpIdentifier("let*", _), _, _) =>
      throw new Exception(s"Invalid Scheme let*: $exp")
    case SExpPair(SExpIdentifier("letrec", _),
                  SExpPair(bindings, SExpPair(first, rest, _), _),
                  _) =>
      SchemeLetrec(compileBindings(bindings),
                   compile(first) :: compileBody(rest),
                   exp.pos)
    case SExpPair(SExpIdentifier("letrec", _), _, _) =>
      throw new Exception(s"Invalid Scheme letrec: $exp")
    case SExpPair(SExpIdentifier("set!", _),
                  SExpPair(SExpIdentifier(variable, _),
                           SExpPair(value, SExpValue(ValueNil, _), _),
                           _),
                  _) =>
      SchemeSet(variable, compile(value), exp.pos)
    case SExpPair(SExpIdentifier("set!", _), _, _) =>
      throw new Exception(s"Invalid Scheme set!: $exp")
    case SExpPair(SExpIdentifier("begin", _), body, _) =>
      SchemeBegin(compileBody(body), exp.pos)
    case SExpPair(SExpIdentifier("cond", _), clauses, _) =>
      SchemeCond(compileCondClauses(clauses), exp.pos)
    case SExpPair(SExpIdentifier("case", _), SExpPair(exp, clauses, _), _) => {
      val (c, d) = compileCaseClauses(clauses)
      SchemeCase(compile(exp), c, d, exp.pos)
    }
    case SExpPair(SExpIdentifier("and", _), args, _) =>
      SchemeAnd(compileBody(args), exp.pos)
    case SExpPair(SExpIdentifier("or", _), args, _) =>
      SchemeOr(compileBody(args), exp.pos)
    case SExpPair(SExpIdentifier("define", _),
                  SExpPair(SExpIdentifier(name, _),
                           SExpPair(value, SExpValue(ValueNil, _), _),
                           _),
                  _) =>
      SchemeDefineVariable(name, compile(value), exp.pos)
    case SExpPair(SExpIdentifier("define", _),
                  SExpPair(SExpPair(SExpIdentifier(name, _), args, _),
                           SExpPair(first, rest, _),
                           _),
                  _) =>
      SchemeDefineFunction(name,
                           compileArgs(args),
                           compile(first) :: compileBody(rest),
                           exp.pos)
    case SExpPair(SExpIdentifier("cas", _),
                  SExpPair(
                  SExpIdentifier(variable, _),
                  SExpPair(eold, SExpPair(enew, SExpValue(ValueNil, _), _), _),
                  _),
                  _) =>
      SchemeCas(variable, compile(eold), compile(enew), exp.pos)
    case SExpPair(SExpIdentifier("cas", _), _, _) =>
      throw new Exception(s"Invalid Scheme cas: $exp")
    case SExpPair(SExpIdentifier("cas-vector", _),
                  SExpPair(
                  SExpIdentifier(variable, _),
                  SExpPair(
                  index,
                  SExpPair(eold, SExpPair(enew, SExpValue(ValueNil, _), _), _),
                  _),
                  _),
                  _) =>
      SchemeCasVector(variable,
                      compile(index),
                      compile(eold),
                      compile(enew),
                      exp.pos)
    case SExpPair(SExpIdentifier("cas-vector", _), _, _) =>
      throw new Exception(s"Indavil Scheme cas-vector: $exp")
    case SExpPair(SExpIdentifier("acquire", _),
                  SExpPair(exp, SExpValue(ValueNil, _), _),
                  _) =>
      SchemeAcquire(compile(exp), exp.pos)
    case SExpPair(SExpIdentifier("acquire", _), _, _) =>
      throw new Exception(s"Invalid Scheme acquire: $exp")
    case SExpPair(SExpIdentifier("release", _),
                  SExpPair(exp, SExpValue(ValueNil, _), _),
                  _) =>
      SchemeRelease(compile(exp), exp.pos)
    case SExpPair(SExpIdentifier("release", _), _, _) =>
      throw new Exception(s"Invalid Scheme release: $exp")
    case SExpPair(SExpIdentifier("spawn", _),
                  SExpPair(exp, SExpValue(ValueNil, _), _),
                  _) =>
      SchemeSpawn(compile(exp), exp.pos)
    case SExpPair(SExpIdentifier("spawn", _), _, _) =>
      throw new Exception(s"Invalid Scheme spawn: $exp")
    case SExpPair(SExpIdentifier("join", _),
                  SExpPair(exp, SExpValue(ValueNil, _), _),
                  _) =>
      SchemeJoin(compile(exp), exp.pos)
    case SExpPair(SExpIdentifier("join", _), _, _) =>
      throw new Exception(s"Invalid Scheme join: $exp")
    case SExpPair(f, args, _) =>
      SchemeFuncall(compile(f), compileBody(args), exp.pos)
    case SExpIdentifier(name, _) =>
      if (reserved.contains(name)) {
        throw new Exception(s"Invalid Scheme identifier (reserved): $exp")
      } else {
        SchemeIdentifier(name, exp.pos)
      }
    case SExpValue(value, _) => SchemeValue(value, exp.pos)
    case SExpQuoted(quoted, _) => SchemeQuoted(quoted, exp.pos)
  }

  def compileArgs(args: SExp): List[String] = args match {
    case SExpPair(SExpIdentifier(id, _), rest, _) => id :: compileArgs(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme argument list: $args")
  }

  def compileBody(body: SExp): List[SchemeExp] = body match {
    case SExpPair(exp, rest, _) => compile(exp) :: compileBody(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme body: $body")
  }

  def compileDoCond(cond: SExp): (SchemeExp, List[SchemeExp]) = cond match {
    case SExpPair(cond, exps, _) =>
      (compile(cond), compileBody(exps))
    case _ =>
      throw new Exception(s"Invalid Scheme do-condition: $cond")
  }

  def compileDoBindings(bindings: SExp): List[(String, SchemeExp, SchemeExp)] =
    bindings match {
      case SExpPair(
          SExpPair(
          SExpIdentifier(name, _),
          SExpPair(init, restOfBinding, _),
          _),
          rest,
          _) =>
        if (reserved.contains(name)) {
          throw new Exception(s"Invalid Scheme identifier (reserved): $name")
        } else {
          val step = restOfBinding match {
            case SExpPair(step, SExpValue(ValueNil, _), _) =>
              compile(step)
            case SExpValue(ValueNil, pos) =>
              SchemeIdentifier(name, pos)
          }
          (name, compile(init), step) :: compileDoBindings(rest)
        }
      case SExpValue(ValueNil, _) => Nil
      case _ => throw new Exception(s"Invalid Scheme bindings: $bindings")
    }

  def compileBindings(bindings: SExp): List[(String, SchemeExp)] =
    bindings match {
      case SExpPair(SExpPair(SExpIdentifier(name, _),
                             SExpPair(value, SExpValue(ValueNil, _), _),
                             _),
                    rest,
                    _) =>
        if (reserved.contains(name)) {
          throw new Exception(s"Invalid Scheme identifier (reserved): $name")
        } else {
          (name, compile(value)) :: compileBindings(rest)
        }
      case SExpValue(ValueNil, _) => Nil
      case _ => throw new Exception(s"Invalid Scheme bindings: $bindings")
    }

  def compileCondClauses(clauses: SExp): List[(SchemeExp, List[SchemeExp])] =
    clauses match {
      case SExpPair(
          SExpPair(SExpIdentifier("else", _), SExpPair(first, rest, _), _),
          SExpValue(ValueNil, _),
          _) =>
        List(
          (SchemeValue(ValueBoolean(true), clauses.pos),
           compile(first) :: compileBody(rest)))
      case SExpPair(SExpPair(cond, SExpPair(first, rest, _), _),
                    restClauses,
                    _) =>
        (compile(cond), compile(first) :: compileBody(rest)) :: compileCondClauses(
          restClauses)
      case SExpPair(SExpPair(cond, SExpValue(ValueNil, _), _),
                    restClauses,
                    _) =>
        (compile(cond), Nil) :: compileCondClauses(restClauses)
      case SExpValue(ValueNil, _) => Nil
      case _ => throw new Exception(s"Invalid Scheme cond clauses: $clauses")
    }

  def compileCaseClauses(clauses: SExp)
    : (List[(List[SchemeValue], List[SchemeExp])], List[SchemeExp]) =
    clauses match {
      case SExpPair(
          SExpPair(SExpIdentifier("else", _), SExpPair(first, rest, _), _),
          SExpValue(ValueNil, _),
          _) =>
        (List(), compile(first) :: compileBody(rest))
      case SExpPair(SExpPair(objects, body, _), restClauses, _) =>
        val (compiled, default) = compileCaseClauses(restClauses)
        ((compileCaseObjects(objects), compileBody(body)) :: compiled, default)
      case SExpValue(ValueNil, _) => (Nil, Nil)
      case _ => throw new Exception(s"Invalid Scheme case clauses: $clauses")
    }

  def compileCaseObjects(objects: SExp): List[SchemeValue] = objects match {
    case SExpPair(SExpValue(v, _), rest, _) =>
      SchemeValue(v, objects.pos) :: compileCaseObjects(rest)
    case SExpPair(SExpIdentifier(id, _), rest, _) =>
      /* identifiers in case expressions are treated as symbols */
      SchemeValue(ValueSymbol(id), objects.pos) :: compileCaseObjects(rest)
    case SExpValue(ValueNil, _) => Nil
    case _ => throw new Exception(s"Invalid Scheme case objects: $objects")
  }
}

object SchemeCompiler extends SchemeCompiler

/**
  * Object that provides a method to compile an s-expression into an Amb Scheme expression.
  * Amb Scheme is identical to regular Scheme, except that it contains amb expressions.
  */
object AmbSchemeCompiler extends SchemeCompiler {

  /*
   * Add the amb keyword to the list of reserved identifiers.
   */
  override val reserved: List[String] = List("amb",
                                             "lambda",
                                             "if",
                                             "let",
                                             "let*",
                                             "letrec",
                                             "cond",
                                             "case",
                                             "set!",
                                             "begin",
                                             "define",
                                             "while",
                                             "cas",
                                             "acquire",
                                             "release")

  override def compile(exp: SExp): SchemeExp = exp match {
    case SExpPair(SExpIdentifier("amb", _), exps, _) =>
      SchemeAmb(compileBody(exps), exp.pos)
    case other =>
      super.compile(other)
  }
}

/**
  * Object that provides a method to rename variables in a Scheme program in
  * order to have only unique names. For example, (let ((x 1)) (let ((x 2)) x))
  * will be converted to (let ((_x0 1)) (let ((_x1 2)) _x1)). This is useful to
  * perform ANF conversion.
  */
object SchemeRenamer {

  /** Maps each variables to their alpha-renamed version (eg. x -> _x0) */
  type NameMap = Map[String, String]

  /** Map each variables to the number of times it is bound */
  type CountMap = Map[String, Int]

  def rename(exp: SchemeExp): SchemeExp =
    rename(exp, Map[String, String](), Map[String, Int]()) match {
      case (e, _) => e
    }

  def rename(exp: SchemeExp,
             names: NameMap,
             count: CountMap): (SchemeExp, CountMap) = exp match {

    case SchemeDo(bindings, test, commands, pos) =>
      countl(bindings.map(_._1), names, count) match {
        case (variables, names1, count1) =>
          val (newBindingsInits, count2) = renameList(bindings.map(_._2), names1, count1)
          val (newBindingsSteps, count3) = renameList(bindings.map(_._3), names1, count2)
          val newBindings = (variables, newBindingsInits, newBindingsSteps).zipped.toList
          val (testCondExp, count4) = rename(test._1, names1, count3)
          val (testExps, count5) = renameList(test._2, names1, count4)
          val newTest = (testCondExp, testExps)
          val (commandsExps, count6) = renameList(commands, names1, count5)
          val newDoExp = SchemeDo(newBindings, newTest, commandsExps, pos)
          (newDoExp, count6)
      }

    case SchemeLambda(args, body, pos) =>
      countl(args, names, count) match {
        case (args1, names1, count1) =>
          renameList(body, names1, count1) match {
            case (body1, count2) => (SchemeLambda(args1, body1, pos), count2)
          }
      }
    case SchemeFuncall(f, args, pos) =>
      rename(f, names, count) match {
        case (f1, count1) =>
          renameList(args, names, count1) match {
            case (args1, count2) => (SchemeFuncall(f1, args1, pos), count2)
          }
      }
    case SchemeIf(cond, cons, alt, pos) =>
      rename(cond, names, count) match {
        case (cond1, count1) =>
          rename(cons, names, count1) match {
            case (cons1, count2) =>
              rename(alt, names, count2) match {
                case (alt1, count3) =>
                  (SchemeIf(cond1, cons1, alt1, pos), count3)
              }
          }
      }
    case SchemeLet(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        /* Use old names for expressions of bindings */
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names, count1) match {
            case (exps, count2) =>
              renameList(body, names1, count2) match {
                case (body1, count3) =>
                  (SchemeLet(variables.zip(exps), body1, pos), count3)
              }
          }
      }
    case SchemeLetStar(bindings, body, pos) =>
      renameLetStarBindings(bindings, names, count) match {
        case (bindings1, names1, count1) =>
          renameList(body, names1, count1) match {
            case (body1, count2) =>
              (SchemeLetStar(bindings1, body1, pos), count2)
          }
      }
    case SchemeLetrec(bindings, body, pos) =>
      countl(bindings.map(_._1), names, count) match {
        /* Use new names for expressions of bindings */
        case (variables, names1, count1) =>
          renameList(bindings.map(_._2), names1, count1) match {
            case (exps, count2) =>
              renameList(body, names1, count2) match {
                case (body1, count3) =>
                  (SchemeLetrec(variables.zip(exps), body1, pos), count3)
              }
          }
      }
    case SchemeSet(variable, value, pos) =>
      rename(value, names, count) match {
        case (value1, count1) =>
          (SchemeSet(names.get(variable) match {
            case Some(n) => n
            case None => variable
          }, value1, pos), count1)
      }
    case SchemeBegin(body, pos) =>
      renameList(body, names, count) match {
        case (body1, count1) => (SchemeBegin(body1, pos), count1)
      }
    case SchemeCond(clauses, pos) =>
      clauses.foldLeft((List[(SchemeExp, List[SchemeExp])](), count))(
        (st: (List[(SchemeExp, List[SchemeExp])], CountMap),
         cl: (SchemeExp, List[SchemeExp])) =>
          (st, cl) match {
            case ((l, cs), (e, body)) =>
              rename(e, names, cs) match {
                case (e1, count1) =>
                  renameList(body, names, count1) match {
                    case (body1, count2) =>
                      ((e1, body1) :: l, count2)
                  }
              }
        }) match {
        case (l, count1) => (SchemeCond(l.reverse, pos), count1)
      }
    case SchemeCase(exp, clauses, default, pos) =>
      rename(exp, names, count) match {
        case (exp1, count1) =>
          clauses.foldLeft(
            (List[(List[SchemeValue], List[SchemeExp])](), count))(
            (st: (List[(List[SchemeValue], List[SchemeExp])], CountMap),
             cl: (List[SchemeValue], List[SchemeExp])) =>
              (st, cl) match {
                case ((l, cs), (objs, body)) =>
                  renameList(body, names, cs) match {
                    case (body1, count1) => ((objs, body1) :: l, count1)
                  }
            }) match {
            case (l, count1) =>
              renameList(default, names, count1) match {
                case (default1, count2) =>
                  (SchemeCase(exp1, l.reverse, default1, pos), count2)
              }
          }
      }
    case SchemeAnd(exps, pos) =>
      renameList(exps, names, count) match {
        case (exps1, count1) => (SchemeAnd(exps1, pos), count1)
      }
    case SchemeOr(exps, pos) =>
      renameList(exps, names, count) match {
        case (exps1, count1) => (SchemeOr(exps1, pos), count1)
      }
    case SchemeDefineVariable(name, value, pos) =>
      /* Keeps name untouched (maybe not correct?) */
      rename(value, names, count) match {
        case (value1, count1) =>
          (SchemeDefineVariable(name, value1, pos), count1)
      }
    case SchemeDefineFunction(name, args, body, pos) =>
      countl(args, names, count) match {
        case (args1, names1, count1) =>
          renameList(body, names1, count1) match {
            case (body1, count2) =>
              (SchemeDefineFunction(name, args1, body1, pos), count2)
          }
      }
    case SchemeQuoted(quoted, pos) =>
      (SchemeQuoted(quoted, pos), count)
    case SchemeIdentifier(name, pos) =>
      names.get(name) match {
        case Some(n) => (SchemeIdentifier(n, pos), count)
        case None => (SchemeIdentifier(name, pos), count) /* keep original name */
      }
    case SchemeValue(v, pos) =>
      (SchemeValue(v, pos), count)
    case _ => throw new Exception(s"Unhandled expression in renamer: $exp")
  }

  /** Renames a list of expressions executed sequentially (eg. within a begin) */
  def renameList(exps: List[SchemeExp],
                 names: NameMap,
                 count: CountMap): (List[SchemeExp], CountMap) = exps match {
    case exp :: rest =>
      val (exp1, count1) = rename(exp, names, count)
      val (rest1, count2) = renameList(rest, names, count1)
      (exp1 :: rest1, count2)
    case Nil => (Nil, count)
  }

  def renameLetStarBindings(
      bindings: List[(String, SchemeExp)],
      names: NameMap,
      count: CountMap): (List[(String, SchemeExp)], NameMap, CountMap) =
    bindings match {
      case (v, e) :: rest =>
        count1(v, names, count) match {
          /* use old names, as with a let* the variable is not yet bound in its
           * definition */
          case (v1, names1, count1) =>
            rename(e, names, count1) match {
              case (e1, count2) =>
                renameLetStarBindings(rest, names1, count2) match {
                  case (rest1, names2, count3) =>
                    ((v1, e1) :: rest1, names2, count3)
                }
            }
        }
      case Nil => (Nil, names, count)
    }

  /** To be called when a new variable is introduced in the scope. Adds it to the
    * name map and count map */
  def count1(variable: String,
             names: NameMap,
             count: CountMap): (String, NameMap, CountMap) = {
    val c: Int = count.get(variable) match {
      case Some(x) => x + 1
      case None => 0
    }
    val n = s"_$variable$c"
    (n, names + (variable -> n), count + (variable -> c))
  }

  /** Same as count1 but for a list of variables */
  def countl(variables: List[String],
             names: NameMap,
             count: CountMap): (List[String], NameMap, CountMap) =
    variables.foldLeft((List[String](), names, count))(
      (st: (List[String], NameMap, CountMap), v: String) =>
        st match {
          case (l, ns, cs) =>
            count1(v, ns, cs) match {
              case (v1, ns1, cs1) => ((v1 :: l), ns1, cs1)
            }
      }) match {
      case (l, ns, cs) => (l.reverse, ns, cs)
    }
}

/**
  * Remove defines from a Scheme expression, replacing them by let bindings.
  * For example:
  *   (define foo 1)
  *   (define (f x) x)
  *   (f foo)
  * Will be converted to:
  *   (letrec ((foo 1)
  *            (f (lambda (x) x)))
  *     (f foo))
  * Which is semantically equivalent with respect to the end result
  */
object SchemeDesugarer {

  var id = 0

  def newVarName(): String = {
    id += 1
    s"#<genvar_$id>"
  }

  def desugar(exps: List[SchemeExp]): SchemeExp = {
    val undefineExp = undefine(exps)
    desugarExp(undefineExp)
  }

  def undefine(exps: List[SchemeExp]): SchemeExp = {
    undefine(exps, List())
  }

  def undefine(exps: List[SchemeExp],
               defs: List[(String, SchemeExp)]): SchemeExp = exps match {
    case Nil => SchemeBegin(Nil, scala.util.parsing.input.NoPosition)
    case SchemeDefineFunction(name, args, body, pos) :: rest =>
      undefine(SchemeDefineVariable(
                 name,
                 SchemeLambda(args, undefineBody(body), exps.head.pos),
                 pos) :: rest,
               defs)
    case SchemeDefineVariable(name, value, _) :: rest =>
      undefine(rest, (name, value) :: defs)
    case _ :: _ =>
      if (defs.isEmpty) {
        undefineBody(exps) match {
          case Nil => SchemeBegin(Nil, scala.util.parsing.input.NoPosition)
          case exp :: Nil => exp
          case exps => SchemeBegin(exps, exps.head.pos)
        }
      } else {
        SchemeLetrec(defs.reverse, undefineBody(exps), exps.head.pos)
      }
  }

  def undefine1(exp: SchemeExp): SchemeExp = undefine(List(exp))

  def undefineBody(exps: List[SchemeExp]): List[SchemeExp] = exps match {
    case Nil => Nil
    case SchemeDefineFunction(_, _, _, _) :: _ => List(undefine(exps, List()))
    case SchemeDefineVariable(_, _, _) :: _ => List(undefine(exps, List()))
    case exp :: rest => {
      val exp2 = exp match {
        case SchemeDo(bindings, test, commands, pos) =>
          SchemeDo(
            bindings.map(binding =>
              (binding._1, undefine1(binding._2), undefine1(binding._3))),
            (undefine1(test._1), undefineBody(test._2)),
            undefineBody(commands),
            pos)
        case SchemeAmb(exps, pos) => SchemeAmb(exps.map(undefine1), pos)
        case SchemeLambda(args, body, pos) =>
          SchemeLambda(args, undefineBody(body), pos)
        case SchemeFuncall(f, args, pos) =>
          SchemeFuncall(undefine1(f), args.map(undefine1), pos)
        case SchemeIf(cond, cons, alt, pos) =>
          SchemeIf(undefine1(cond), undefine1(cons), undefine1(alt), pos)
        case SchemeLet(bindings, body, pos) =>
          SchemeLet(bindings.map({ case (b, v) => (b, undefine1(v)) }),
                    undefineBody(body),
                    pos)
        case SchemeLetStar(bindings, body, pos) =>
          SchemeLetStar(bindings.map({ case (b, v) => (b, undefine1(v)) }),
                        undefineBody(body),
                        pos)
        case SchemeLetrec(bindings, body, pos) =>
          SchemeLetrec(bindings.map({ case (b, v) => (b, undefine1(v)) }),
                       undefineBody(body),
                       pos)
        case SchemeSet(variable, value, pos) =>
          SchemeSet(variable, undefine1(value), pos)
        case SchemeBegin(exps, pos) => SchemeBegin(undefineBody(exps), pos)
        case SchemeCond(clauses, pos) =>
          SchemeCond(clauses.map({
            case (cond, body) => (undefine1(cond), undefineBody(body))
          }), pos)
        case SchemeCase(key, clauses, default, pos) =>
          SchemeCase(undefine1(key), clauses.map({
            case (vs, body) => (vs, undefineBody(body))
          }), undefineBody(default), pos)
        case SchemeAnd(args, pos) => SchemeAnd(args.map(undefine1), pos)
        case SchemeOr(args, pos) => SchemeOr(args.map(undefine1), pos)
        case SchemeIdentifier(name, pos) => SchemeIdentifier(name, pos)
        case SchemeQuoted(quoted, pos) => SchemeQuoted(quoted, pos)
        case SchemeValue(value, pos) => SchemeValue(value, pos)
        case SchemeCas(variable, eold, enew, pos) =>
          SchemeCas(variable, undefine1(eold), undefine1(enew), pos)
        case SchemeCasVector(variable, index, eold, enew, pos) =>
          SchemeCasVector(variable,
                          undefine1(index),
                          undefine1(eold),
                          undefine1(enew),
                          pos)
        case SchemeAcquire(exp, pos) => SchemeAcquire(undefine1(exp), pos)
        case SchemeRelease(exp, pos) => SchemeRelease(undefine1(exp), pos)
        case SchemeSpawn(exp, pos) => SchemeSpawn(undefine1(exp), pos)
        case SchemeJoin(exp, pos) => SchemeJoin(undefine1(exp), pos)
      }
      exp2 :: undefineBody(rest)
    }
  }

  /**
    * Desugars single expressions, e.g. and-, or- and cond-expressions.
    *
    * (and a b c) is syntactic sugar for:
    * (if a
    *     (if b
    *         c
    *         #f)
    *     #f)
    *
    * (or a b c) is syntactic sugar for:
    * (let ((va a))
    *   (if va
    *       va
    *       (let ((vb b))
    *         (if vb
    *             vb
    *             c))))
    *
    * (cond (a u v w)
    *       (else x y z))
    * is syntactic sugar for:
    * (if a
    *     (begin u v w)
    *     (begin x y z))
    */
  def desugarExp(exp: SchemeExp): SchemeExp = {
    exp match {
      case SchemeAnd(exps, pos) =>
        exps match {
          case Nil => SchemeValue(ValueBoolean(true), pos)
          case andExp :: Nil => desugarExp(andExp)
          case andExp :: rest =>
            SchemeIf(desugarExp(andExp),
                     desugarExp(SchemeAnd(rest, exp.pos)),
                     SchemeValue(ValueBoolean(false), pos),
                     pos)
        }
      case SchemeOr(exps, pos) =>
        exps match {
          case Nil => SchemeValue(ValueBoolean(false), pos)
          case orExp :: Nil => desugarExp(orExp)
          case orExp :: rest =>
            /*
             * Generate a unique variable name by combining characters (#<>) that can't be used inside Scheme identifiers
             * by users with a unique id.
             */
            val tempVarName = newVarName()
            val body: SchemeExp = SchemeIf(SchemeIdentifier(tempVarName, pos),
                                           SchemeIdentifier(tempVarName, pos),
                                           desugarExp(SchemeOr(rest, exp.pos)),
                                           pos)
            SchemeLet(List((tempVarName, desugarExp(orExp))), List(body), pos)
        }

      case SchemeCond(clauses, pos) =>
        clauses match {
          case Nil => SchemeValue(ValueBoolean(false), pos)
          case (cond, body) :: rest =>
            val desugaredRest = desugarExp(SchemeCond(rest, exp.pos))
            val desugaredBody = body match {
              case Nil => SchemeBegin(Nil, pos)
              case head :: bodyRest =>
                SchemeBegin(body.map(desugarExp), head.pos)
            }
            desugarExp(cond) match {
              case SchemeIdentifier("else", pos) =>
                SchemeIf(SchemeValue(ValueBoolean(true), pos),
                         desugaredBody,
                         desugaredRest,
                         pos)
              case other =>
                SchemeIf(other, desugaredBody, desugaredRest, pos)
            }
        }

      case SchemeDo(bindings, test, commands, pos) =>
        val uniqueName = newVarName()
        val vars = bindings.map(_._1)
        val inits = bindings.map(_._2).map(desugarExp)
        val steps = bindings.map(_._3).map(desugarExp)
        val endExpressions =
          if (test._2.size == 1) desugarExp(test._2.head) else SchemeBegin(test._2.map(desugarExp), pos)
        val recursiveCall =
          SchemeFuncall(SchemeIdentifier(uniqueName, pos), steps, pos)
        val lambda = SchemeLambda(
          vars,
          List(
            SchemeIf(test._1,
                     endExpressions,
                     SchemeBegin(commands.map(desugarExp) :+ recursiveCall, pos),
                     pos)),
          pos)
        SchemeLetrec(
          List((uniqueName, lambda)),
          List(SchemeFuncall(SchemeIdentifier(uniqueName, pos), inits, pos)),
          pos)
      case SchemeAmb(exps, pos) => SchemeAmb(exps.map(desugarExp), pos)
      case SchemeLambda(args, body, pos) =>
        SchemeLambda(args, body.map(desugarExp), pos)
      case SchemeFuncall(f, args, pos) =>
        SchemeFuncall(desugarExp(f), args.map(desugarExp), pos)
      case SchemeIf(cond, cons, alt, pos) =>
        SchemeIf(desugarExp(cond), desugarExp(cons), desugarExp(alt), pos)
      case SchemeLet(bindings, body, pos) =>
        SchemeLet(bindings.map({ case (b, v) => (b, desugarExp(v)) }),
                  body.map(desugarExp),
                  pos)
      case SchemeLetStar(bindings, body, pos) =>
        SchemeLetStar(bindings.map({ case (b, v) => (b, desugarExp(v)) }),
                      body.map(desugarExp),
                      pos)
      case SchemeLetrec(bindings, body, pos) =>
        SchemeLetrec(bindings.map({ case (b, v) => (b, desugarExp(v)) }),
                     body.map(desugarExp),
                     pos)
      case SchemeSet(variable, value, pos) =>
        SchemeSet(variable, desugarExp(value), pos)
      case SchemeBegin(exps, pos) => SchemeBegin(exps.map(desugarExp), pos)
      case SchemeCond(clauses, pos) =>
        SchemeCond(clauses.map({
          case (cond, body) => (undefine1(cond), undefineBody(body))
        }), pos)
      case SchemeCase(key, clauses, default, pos) =>
        SchemeCase(desugarExp(key), clauses.map({
          case (vs, body) => (vs, body.map(desugarExp))
        }), default.map(desugarExp), pos)
      case SchemeAnd(args, pos) => SchemeAnd(args.map(undefine1), pos)
      case SchemeOr(args, pos) => SchemeOr(args.map(undefine1), pos)
      case SchemeIdentifier(name, pos) => SchemeIdentifier(name, pos)
      case SchemeQuoted(quoted, pos) => SchemeQuoted(quoted, pos)
      case SchemeValue(value, pos) => SchemeValue(value, pos)
      case SchemeCas(variable, eold, enew, pos) =>
        SchemeCas(variable, eold, enew, pos)
      case SchemeCasVector(variable, index, eold, enew, pos) =>
        SchemeCasVector(variable,
                        desugarExp(index),
                        desugarExp(eold),
                        desugarExp(enew),
                        pos)
      case SchemeAcquire(variable, pos) => SchemeAcquire(variable, pos)
      case SchemeRelease(variable, pos) => SchemeRelease(variable, pos)
      case SchemeSpawn(exp, pos) => SchemeSpawn(desugarExp(exp), pos)
      case SchemeJoin(exp, pos) => SchemeJoin(desugarExp(exp), pos)
    }
  }
}

trait Scheme {

  /**
    * Compiles a s-expression into a scheme expression
    */
  def compile(exp: SExp): SchemeExp = SchemeCompiler.compile(exp)

  /**
    * Performs alpha-renaming to ensure that every variable has a unique name
    */
  def rename(exp: SchemeExp): SchemeExp = SchemeRenamer.rename(exp)

  /**
    * Replace defines in a program (a list of expressions) by a big letrec as a single expression
    */
  def desugar(exps: List[SchemeExp]): SchemeExp = SchemeDesugarer.desugar(exps)

  /**
    * Parse a string representing a Scheme program
    */
  def parse(s: String): SchemeExp = desugar(SExpParser.parse(s).map(compile _))
}

object Scheme extends Scheme

object AmbScheme extends Scheme {

  override def compile(exp: SExp): SchemeExp = AmbSchemeCompiler.compile(exp)
}
