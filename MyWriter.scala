/**
 * Implementation of cat's Writer class along with Monad, Functor, Monoid and Semigroup
 */
object MyWriter extends App {
  import Syntax._
  import Instances._

  println(empty[String] + "!")
  println(combineAll(List("a", "b")))
  println(combineAll(List.empty[String]))


  type MMW[A] = Writer[List[String], A]
  val x = pure[Int, MMW](5)
  println(x)

  val result = for {
    five <- Writer(List("five"), 5)
    two <- Writer(List("two"), 2)
    _ <- Writer.log(List("more", "things", "here"))
  } yield five + two

  println("Result = " + result)
}

object Instances {
  implicit val stingSemigroup: Semigroup[String] = _ + _

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    override def empty: String = ""

    override def combine(a: String, b: String): String = stingSemigroup.combine(a, b)
  }

  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List.empty

    override def flatMap[A, B](v: List[A])(f: A => List[B]): List[B] = v.flatMap(f)
  }

  implicit def listMonoid[T]: Monoid[List[T]] = new Monoid[List[T]] {
    override def empty: List[T] = List.empty

    override def combine(a: List[T], b: List[T]): List[T] = a ++ b
  }

  implicit def writerMonad[L](implicit m: Monoid[L]): Monad[({type W[A] = Writer[L, A]})#W] =
    new Monad[({ type W[A] = Writer[L, A]})#W] {
      override def pure[A](a: A): Writer[L, A] = Writer(m.empty, a)

      override def flatMap[A, B](v: Writer[L, A])(f: A => Writer[L, B]): Writer[L, B] = {
        val value = f(v.value)
        value.copy(log = m.combine(value.log, v.log))
      }
    }
}

object Syntax {
  def empty[T](implicit monoid: Monoid[T]): T = monoid.empty

  def pure[A, T[_]](a: A)(implicit monad: Monad[T]): T[A] = monad.pure(a)

  def combineAll[T](list: List[T])(implicit monoid: Monoid[T]): T = list.foldRight(monoid.empty)(monoid.combine)

  implicit class FlatMap[T[_], A](t: T[A])(implicit monad: Monad[T]) {
    def flatMap[B](f: A => T[B]): T[B] = monad.flatMap(t)(f)
  }

  implicit class Map[T[_], A](t: T[A])(implicit functor: Functor[T]) {
    def map[B](f: A => B) = functor.map(t)(f)
  }
}

trait Semigroup[T] {
  def combine(a: T, b: T): T
}

trait Monoid[T] extends Semigroup[T] {
  def empty: T
}

trait Functor[F[_]] {
  def map[T, A](v: F[T])(f: T => A): F[A]
}

trait Monad[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def flatMap[A, B](v: F[A])(f: A => F[B]): F[B]

  override def map[T, A](v: F[T])(f: T => A): F[A] = flatMap(v)(t => pure(f(t)))

}

case class Writer[L, V](log: L, value: V)

object Writer {
  def log[T](l: T): Writer[T, Unit] = Writer(l, ())
  def value[L, T](t: T)(implicit monoid: Monoid[L]): Writer[L, T] = Writer(monoid.empty, t)
}
