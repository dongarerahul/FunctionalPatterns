package patterns

trait Functor[C[_]] {
  def map[A, B](f : A => B): C[A] => C[B]
}

trait Applicative[F[_]] {
  def apply[A, B](f: F[A => B]): F[A] => F[B]
}

object ApplicativeVsMonad {

  case class Foo(s: Symbol, n: Int)
  val whatever = "Welcome"

  def maybeComputeS(whatever: String) : Option[Symbol] = Some(Symbol(whatever))
  def maybeComputeN(whatever: String) : Option[Int] = if (whatever == null) None else Some(whatever.length + 1)

  val mayBeFoo = for {
    s <- maybeComputeS(whatever)
    n <- maybeComputeN(whatever)
  } yield Foo(s, n)

  /** *
    * Here we can see Monad Option[Symbol] return by maybeComputeS and Monad Option[Int]
    * don't have any dependency among them. But
    * Here, maybeComputeN will be evaluated only after maybeComputeSmayBeFoo1
    *
    * In case, we don't want two evaluation independent of each other,
    * we can applicative functor
    */
  val mayBeFoo1 = maybeComputeS(whatever).flatMap(s => maybeComputeN(whatever).map(n => Foo(s, n)))

  (Applicative[Option] apply (Functor[Option] map g)(Option(5)))(Option(10))

  def main(args: Array[String]) {

    println(mayBeFoo)
    println(mayBeFoo1)
  }
}
