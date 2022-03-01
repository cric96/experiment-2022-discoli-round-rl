package it.unibo.casestudy.utils

import scala.collection.mutable

object Memoize {
  def apply[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }
}
