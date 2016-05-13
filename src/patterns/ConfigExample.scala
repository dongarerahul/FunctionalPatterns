package patterns

object ConfigExample {
  def main(args: Array[String]) {

  }
}

//Functor type
trait Functor[F[_]] {
  def apply[A](a: A) : F[A]
  def map[A, B](x: F[A])(f: A => B) : F[B]
}

trait Monad[M[_]] {
  def flatten[A](m: M[M[A]]) : M[A]
  def flatMap[A, B](m: M[A])(f: A => M[B])(implicit func: Functor[M]) : M[B] = flatten(func.map(m)(f))
}

trait Config[+A] {
  def map[B](f: A => B) : Config[B]
  def flatMap[B](f: A => Config[B]) : Config[B]
  def get : A
}

object Config {
  def apply[A](data: => A) = new Config[A] {
    def get = data

    override def map[B](f: A => B): Config[B] = ???

    override def flatMap[B](f: (A) => Config[B]): Config[B] = ???
  }
}

object ConfigAsFunctor extends Functor[Config] {
  override def apply[A](a: A): Config[A] = Config(a)
  override def map[A, B](a: Config[A])(f: A => B): Config[B] = a.map(f)

  implicit def functorOps[F[_] : Functor, A](ma: F[A]) = new {
    val functor : Functor[F] = implicitly[Functor[F]]
    final def map[B](f: A => B) : F[B] = functor.map(ma)(f)
  }

  def lift[F[_]: Functor] = new {
    val functor = implicitly[Functor[F]]
    def apply3[A, B, C, D](f: (A, B, C) => D): (F[A], F[B], F[C]) => F[F[F[D]]] = {
      (fa, fb, fc) => fa map(a => fb map(b => fc map(c => f(a, b, c))))
    }
  }
}