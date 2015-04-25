package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b.apply()*b.apply() - 4*a.apply()*c.apply())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {

    Signal(
      computeDelta(a,b,c).apply() match {
        case imaginary if imaginary < 0 => Set()
        case single if single == 0 => Set(-b.apply()/(2*a.apply()))
        case _ => Set( (-b.apply() + Math.sqrt(computeDelta(a,b,c).apply()))/(2*a.apply()) , (-b.apply() - Math.sqrt(computeDelta(a,b,c).apply()))/(2*a.apply()))
      }
    )
  }
}
