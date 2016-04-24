package functors

/***
  * a) ( A=>B )    => ( C[A]=>C[B] )   | Functor
  * b) ( A=>C[B] ) => ( C[A]=>C[B] )   | Monad
  * c) ( C[A=>B] ) => ( C[A]=>C[B] )   | Applicative
 ***/
object Monads {

  val boxedString = new MyBox1[String]("Hello")

  def main(args: Array[String]) {
    val monadEx: MyBox1[Int] = flatMap[String, Int](boxLengthOf)(boxedString)

    println("My First Monad : flatMap(rawLengthOf)(boxedString) : " + monadEx)

  }

  def boxLengthOf(a: String) : MyBox1[Int] = new MyBox1(rawLengthOf(a))
  def rawLengthOf(a: String) : Int = a.length

  def flatMap[A, B](semiRawFunction: A => MyBox1[B]) : MyBox1[A] => MyBox1[B] = {
    myBox: MyBox1[A] => semiRawFunction(myBox.value)
  }
}

case class MyBox1[T](value: T)