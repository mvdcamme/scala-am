object ActionLogger {

  /*
   * The list of all actions that were logged.
   * We store them as a list of Objects, because we only want to print them anyway.
   */
  var ACTIONS_EXECUTED: List[Object] = List()

  def logAction[Exp, Abs, Addr](action: ActionT[Exp, Abs, Addr]): Unit = {
    if (GlobalFlags.PRINT_ACTIONS_EXECUTED) {
      ACTIONS_EXECUTED = ACTIONS_EXECUTED :+ action
    }
  }

  def printActions(): Unit = {
    Logger.log("####### actions executed #######", Logger.E)
    for (ac <- ACTIONS_EXECUTED) {
      Logger.log(ac, Logger.E)
    }
    Logger.log("####### actions executed #######", Logger.E)
  }

}
