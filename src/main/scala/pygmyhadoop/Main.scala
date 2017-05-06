package pygmyhadoop

import cats.data.Validated
import cats.instances.either._
import cats.instances.int._
import cats.instances.list._
import cats.syntax.either._
import pygmyhadoop.foldmap.naturalTransformation.foldMap

import scala.language.{higherKinds, postfixOps}

object Main extends App {
  type ErrorOr[A] = Either[NumberFormatException, A]
  type AllErrorsOr[A] = Validated[List[NumberFormatException], A]

  println(
    foldMap[String, ErrorOr, AllErrorsOr, Int]
      (List("1", "2", "3"))
      (a => Either.catchOnly[NumberFormatException](a.toInt))
      ((f: ErrorOr[Int]) => f.toValidated.leftMap(List(_)))
  )

  println(
    foldMap[String, ErrorOr, AllErrorsOr, Int]
      (List("a", "2", "c"))
      (a => Either.catchOnly[NumberFormatException](a.toInt))
      ((f: ErrorOr[Int]) => f.toValidated.leftMap(List(_)))
  )

  println(
    foldMap[String, ErrorOr, ErrorOr, Int]
      (List("a", "2", "c"))
      (a => Either.catchOnly[NumberFormatException](a.toInt))
      ()
  )
}
