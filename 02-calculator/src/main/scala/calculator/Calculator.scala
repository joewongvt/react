package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.mapValues( e => Signal(eval(e(), namedExpressions.filter( (kv:(String, Signal[Expr])) => kv._2 != e ))))
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case l:Literal => l.v
      case r:Ref     => eval(getReferenceExpr(r.name, references), references)
      case p:Plus    => eval(p.a, filterOut(p, references)) + eval(p.b, filterOut(p, references))
      case m:Minus   => eval(m.a, filterOut(m, references)) - eval(m.b, filterOut(m, references))
      case t:Times   => eval(t.a, filterOut(t, references)) * eval(t.b, filterOut(t, references))
      case d:Divide  => eval(d.a, filterOut(d, references)) / eval(d.b, filterOut(d, references))
      case _         => Double.NaN
    }
  }

  def filterOut(e:Expr, references: Map[String, Signal[Expr]]): Map[String, Signal[Expr]] = {
    references.filter( (kv:(String, Signal[Expr])) => kv._2() != e)
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
