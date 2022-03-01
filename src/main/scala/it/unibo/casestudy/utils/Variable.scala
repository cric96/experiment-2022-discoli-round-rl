package it.unibo.casestudy.utils

trait Variable[T] {
  def value: T
  def next(): Variable[T] // side effect
}

object Variable {
  implicit def toBase[T](variable: Variable[T]): T = variable.value
  implicit def fromStandard[T](data: T): Variable[T] = some(data)

  type V[T] = Variable[T]

  def some[T](data: T): Variable[T] = new V[T] {
    override def value: T = data
    override def next(): V[T] = this
  }

  def changeAfter[T](ticks: Int, initData: T, andThen: T): V[T] = new V[T] {
    require(ticks >= 0)
    private var clock = ticks
    override def value: T = if (ticks == 0) {
      andThen
    } else {
      initData
    }

    override def next(): V[T] = {
      if (clock > 0) {
        clock -= 1
      }
      this
    }
  }

  def linearDecay[T: Numeric](initData: T, decayFactor: T): V[T] = new V[T] {
    private var _value = initData
    override def value: T = _value

    override def next(): V[T] = {
      _value = Numeric[T].minus(_value, decayFactor)
      this
    }
  }

}
