package functors

/***
  * a) ( A=>B )    => ( C[A]=>C[B] )   | Functor
  * b) ( A=>C[B] ) => ( C[A]=>C[B] )   | Monad
  * c) ( C[A=>B] ) => ( C[A]=>C[B] )   | Applicative
  ***/
object Applicatives {

  val boxedString = new MyBox2[String]("Hello")

  def main(args: Array[String]) {

    val applicative = apply[String, Int](boxedLengthOf)(boxedString)

    println("My First Applicative : flatMap(rawLengthOf)(boxedString) : " + applicative)
    
  }

  def lengthOf(a: String) : Int = a.length
  val boxedLengthOf = new MyBox2[String => Int](lengthOf)

  def apply[A, B](f: MyBox2[A => B]) : MyBox2[A] => MyBox2[B] = {
    myBox: MyBox2[A] => new MyBox2(f.value(myBox.value))
  }
}

case class MyBox2[T](value: T)