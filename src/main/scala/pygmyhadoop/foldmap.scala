package pygmyhadoop

import cats._
import cats.syntax.applicative._
import cats.syntax.cartesian._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.semigroup._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.higherKinds

object foldmap {
  object sequential {
    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B = identity[A](_)): B = monadic.foldMap(it)(f(_).pure[Id])
  }

  object parallel {
    def foldMap[A, B: Monoid](it: Iterable[A])(f: A => B = identity[A](_)): Future[B] = {
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
      for {
        sum <- m
        b <- f(a)
      } yield sum |+| b
    }
  }

  object naturalTransformation {
    def foldMap[A, F[_]: Monad, G[_]: Applicative, B: Monoid]
    (it: Iterable[A])
    (nt: F[B] => G[B] = identity[F[B]](_))
    (func: A => F[B] = (a: A) => a.pure[Id])
    : G[B] = it.foldLeft(Monoid[B].empty.pure[G]){ (g, a) =>
      (g |@| nt(func(a))).map(_ |+| _)
    }
  }
}
