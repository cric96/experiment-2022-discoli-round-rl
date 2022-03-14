package it.unibo.casestudy.utils

import scala.collection.mutable

object Memoize {
  /** Memoization functionality
    * @param f
    *   the function to memoize
    * @tparam I
    *   the input type
    * @tparam O
    *   the output type
    * @return
    *   the memoized function
    */
  def apply[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }
}
