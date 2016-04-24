package patterns

/***
  * a) ( A=>B )    => ( C[A]=>C[B] )   | Functor
  * b) ( A=>C[B] ) => ( C[A]=>C[B] )   | Monad
  * c) ( C[A=>B] ) => ( C[A]=>C[B] )   | Applicative
  ***/

object Functors {

  val boxedString = new MyBox[String]("Hello")

  def main(args: Array[String]) {
    println("My First Functor : map(rawLengthOf)(boxedString) : " + map(rawLengthOf)(boxedString))

    val result = new MyBox2[String]("Welcome")
    println ("Simple Usage of Functor: " )
  }

  def lengthOf(box: MyBox[String]) : MyBox[Int] = ???
  def rawLengthOf(a: String) = a.length
  def map[A, B](rawFunction: A => B) : MyBox[A] => MyBox[B] = {
    myBox: MyBox[A] => new MyBox(rawFunction(myBox.value))
  }
}

case class MyBox[T](value: T)