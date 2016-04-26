package patterns

case class LogBox[T](value: T, message: String = "") {
  def map[B](f: T => B) : LogBox[B] = new LogBox(f(value), message)
  def flatten[B](box: LogBox[LogBox[B]]) = new LogBox[B](box.value.value, box.message ++ box.value.message)
  def flatMap[B](f: T => LogBox[B]) : LogBox[B] = flatten(map(f))
}

object ConceptsApply {
  def main(args: Array[String]) {
    //val box1 = LogBox(1)
    val box2 = LogBox(2, "Second Message | ")

    //map magic
    //Interesting facts: Even after applying map, original box value is passed onto new one
    val result = box2.map(value => value * 10)
    println(result)

    //flatMap magic
    val box3 = box2.flatMap(v => new LogBox(3, "Third Message | "))
    val box4 = box3.flatMap(v => new LogBox(4, "Fourth Message | "))
    println("Using FlatMap: " + box4)

    //syntactic Sugar
    val sugared = for {
      a <- LogBox(1)
      b <- LogBox(2, "Hello ")
      c <- LogBox(3, "World ! ")
    } yield (c)

    println("Sugared: " + sugared)

    // in a flatMap fashion
    val deSugared = LogBox(1, "Hello ").flatMap(b => LogBox(2, "World !").map(c => c))
    println("Desugared: " + deSugared)
  }
}
