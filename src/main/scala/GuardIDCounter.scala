object GuardIDCounter {

  var counter = 0

  def incCounter(): Integer = {
    val temp = counter
    counter += 1
    temp
  }

}
