object Stopwatch {

  var lastResumeTime: Long = 0
  var totalTime: Long = 0

  private def timeSinceLastResume: Long =
    System.nanoTime() - lastResumeTime

  def start(): Unit =
    resume()

  def stop: Double = {
    pause()
    time
  }

  protected def pause(): Unit =
    totalTime += timeSinceLastResume

  protected def resume(): Unit =
    lastResumeTime = System.nanoTime()

  def doPaused[A](block: => A): A = {
    pause()
    val result = block
    resume()
    result
  }

  def time: Double =
    totalTime / Math.pow(10, 9)

}
