import backend.path_filtering.PartialRegexMatcher

trait TestsPartialMatchers {

  private def resetRandom: scala.util.Random = {
    new scala.util.Random(System.nanoTime())
  }

  def generateRandomPaths(nrOfPaths: Int, maxPathSize: Int): List[String] = {
    val random = resetRandom
    1.to(nrOfPaths).map(_ => {
      val randomSize: Int = random.nextInt(maxPathSize) + 1
      1.to(randomSize).map(_ => if (random.nextInt(2) == 0) "t" else "e").mkString
    }).toList
  }

  def checkPartialMatchersEqual(pm1: PartialRegexMatcher, pm2: PartialRegexMatcher): Unit = {
    val randomPaths = generateRandomPaths(2000, 10)
    randomPaths.foreach(path => {
      val (result1, _) = pm1.incrementalMatch(path)
      val (result2, _) = pm2.incrementalMatch(path)
      println(path)
      if (result1 != result2) {
        val (r1, _) = pm1.incrementalMatch(path)
        val (r2, _) = pm2.incrementalMatch(path)
      }
      assert(result1 == result2)
    })
  }

}
