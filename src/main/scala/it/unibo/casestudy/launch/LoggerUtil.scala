package it.unibo.casestudy.launch

import scribe.Level

object LoggerUtil {
  /** Disable the logger if a certain condition is meet
    * @param condition
    *   the condition used to disable the logger
    */
  def disableIf(condition: Boolean): Unit = if (condition) {
    scribe.Logger.root
      .clearHandlers()
      .clearModifiers()
      .withHandler(minimumLevel = Some(Level.Warn))
      .replace()
  }
}
