// An expression evaluator 
@main def scala2Eval: Unit =

  type Env = Map[String, Int]
  type WithEnv = Env ?=> Int
  
  def summonEnv: Env ?=> Env = summon[Env]

  enum Exp:
    case Val(value: Int)
    case Add(left: Exp, right: Exp)
    case Mul(left: Exp, right: Exp)
    case Var(identifier: String)

  import Exp._

  def eval(exp: Exp): WithEnv =
    exp match {
      case Var(id) => handleVar(id)
      case Val(value) => value
      case Add(l, r) => handleAdd(l, r)
      case Mul(l, r) => handleMul(l, r)
    }
  
  def handleAdd(l: Exp, r: Exp): WithEnv = eval(l) + eval(r)
  def handleMul(l: Exp, r: Exp): WithEnv = eval(l) * eval(r)
  def handleVar(s: String): WithEnv = summonEnv.getOrElse(s, 0)

  val exp1: Exp =
    Mul(
      Var("z"),
      Add(
        Val(30),
        Mul(
          Var("x"),
          Var("y"))
      )
    )

  given env as Env = Map("x" -> 17, "y" -> 10, "z" -> 2)
  
  val eval1 = eval(exp1)

  assert(eval1 == 400)
  println(s"Eval exp gives $eval1")
