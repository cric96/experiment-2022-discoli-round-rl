package it.unibo.casestudy.utils

object UnsafeProduct {
  implicit class UnsafeProduct(val p: Product) extends AnyVal {
    def _1[T]: T = get(0)
    def _2[T]: T = get(1)
    def _3[T]: T = get(2)
    def _4[T]: T = get(3)
    def _5[T]: T = get(4)
    def _6[T]: T = get(5)
    def _7[T]: T = get(6)
    def _8[T]: T = get(7)
    private def get[T](i: Int): T = p.productElement(i).asInstanceOf[T]
  }
}
