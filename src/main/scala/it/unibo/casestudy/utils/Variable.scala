package it.unibo.casestudy.utils

trait Variable[T] {
  def value: T
  def next(): Variable[T] // side effect
}

object Variable {
  implicit def toBase[T](variable: Variable[T]): T = variable.value
  type V[T] = Variable[T]
}
