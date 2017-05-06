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
  type ErrorsOr[A] = Validated[List[NumberFormatException], A]

  println(
    foldMap[String, ErrorOr, ErrorsOr, Int]
      (List("1", "2", "3"))
      ((f: ErrorOr[Int]) => f.toValidated.leftMap(List(_)))
      (a => Either.catchOnly[NumberFormatException](a.toInt))
  )

  println(
    foldMap[String, ErrorOr, ErrorsOr, Int]
      (List("a", "2", "c"))
      ((f: ErrorOr[Int]) => f.toValidated.leftMap(List(_)))
      (a => Either.catchOnly[NumberFormatException](a.toInt))
  )
}
