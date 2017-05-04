package pygmyhadoop

import cats._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

object foldmap {
  object sequential {
    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B = identity[A] _): B = it.foldLeft(Monoid[B].empty)(_ |+| f(_))
//    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B): B = it.foldLeft(Monoid[B].empty)(_ |+| f(_))
//    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B): B = monadic.foldMap(it)(f(_).pure[Id])
  }

  object parallel {
    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B = identity[A] _): Future[B] = {
//    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B): Future[B] = {
      val numCores: Int = Runtime.getRuntime.availableProcessors
      val groupSize: Int = (1.0 * it.size / numCores).ceil.toInt
      val groups: Iterator[Iterable[A]] = it.grouped(groupSize)

      val futures: Iterator[Future[B]] = groups map (g =>
        Future(g.foldLeft(Monoid[B].empty)(_ |+| f(_)))
      )

      Future.sequence(futures).map(_.foldLeft(Monoid[B].empty)(_ |+| _))
    }
  }

  object monadic {
    def foldMap[A, M[_]: Monad, B: Monoid](it: Iterable[A])(f: A => M[B] = (a: A) => a.pure[Id]): M[B] = it.foldLeft(Monoid[B].empty.pure[M]){ (m, a) =>
//    def foldMap[A, M[_]: Monad, B: Monoid](it: Iterable[A])(f: A => M[B]): M[B] = it.foldLeft(Monoid[B].empty.pure[M]){ (m, a) =>
      for {
        sum <- m
        b <- f(a)
      } yield sum |+| b
    }
  }
}
