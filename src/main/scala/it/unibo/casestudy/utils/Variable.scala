package it.unibo.casestudy.utils

/** An object that wraps a data that could changes with a defined logic
  * @tparam T
  *   the type of the wrapped data
  */
trait Variable[T] {
  /** @return the current state of the variable */
  def value: T

  /** @return produces a side effects and then returns the new variable state */
  def next(): Variable[T] // side effect

  override def toString: String = s"V($value)"
}

object Variable {
  // Type conversion
  implicit def toBase[T](variable: Variable[T]): T = variable.value
  implicit def fromStandard[T](data: T): Variable[T] = some(data)
  // Utility to mark variable data
  type V[T] = Variable[T]

  /** A simple wrapper, returns always the same value
    * @param data
    *   the data contained in the variable
    * @tparam T
    *   the data type
    * @return
    *   the wrapper object created
    */
  def some[T](data: T): Variable[T] = new V[T] {
    override def value: T = data
    override def next(): V[T] = this
  }

  /** A data that changes only after a certain number of next calls
    * @param ticks
    *   the number of next call
    * @param initData
    *   the initial data type
    * @param andThen
    *   the data returned after ticks call
    * @tparam T
    *   the data type
    * @return
    *   the wrapper object created
    */
  def changeAfter[T](ticks: Int, initData: T, andThen: T): V[T] = new V[T] {
    private var clock = ticks
    require(clock >= 0)
    override def value: T = if (clock == 0) {
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

  /** Change the data at each next call, removing the decay factor
    * @param initData
    *   the initial data value
    * @param decayFactor
    *   how the data should be updated
    * @tparam T
    *   the wrapped data type
    * @return
    *   the wrapper object created
    */
  def linearDecay[T: Numeric](initData: T, decayFactor: T): V[T] = new V[T] {
    private var _value = initData
    override def value: T = _value

    override def next(): V[T] = {
      _value = Numeric[T].minus(_value, decayFactor)
      this
    }
  }

  /** Change the data with a certain logic that could changes in each new tick
    * @param initData
    *   the initial data value
    * @param evolve
    *   the logic that associated for each next call a new value (i: Int => T)
    * @tparam T
    *   the wrapped data type
    * @return
    */
  def evolveWith[T](initData: T, evolve: Int => T): V[T] = new V[T] {
    private var i = 0
    private var _data = initData
    override def value: T = _data

    override def next(): V[T] = {
      i += 1
      _data = evolve(i)
      this
    }
  }
}
