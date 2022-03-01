package it.unibo.casestudy.utils

object RichDouble {
  implicit class TypeEnrichment(val d: Double) {
    def default = 0.00001
    def ~=(d2: Double) = (d - d2).abs < default
  }
}
