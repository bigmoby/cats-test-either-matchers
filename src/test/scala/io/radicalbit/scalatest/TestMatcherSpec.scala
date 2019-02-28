package io.radicalbit.scalatest

import cats.data.EitherT
import cats.implicits._
import io.radicalbit.scalatest.CatsEitherMatchers._
import org.scalatest.{MustMatchers, WordSpecLike}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.{FiniteDuration, _}
import scala.language.postfixOps

class TestMatcherSpec extends WordSpecLike with MustMatchers {

  implicit val duration: FiniteDuration = 10 seconds

  case class Box(value1: Int, value2: String)

  val rightTest: Either[_, Box] = Right(Box(3, "BigMoby"))

  val leftTest: Either[RuntimeException, _] = Left(
    new RuntimeException("BOOOM!")
  )

  val rightFromEitherTTest: EitherT[Future, Throwable, Box] =
    EitherT.pure[Future, Throwable](Box(3, "BigMoby"))

  val leftFromEitherTTest: EitherT[Future, RuntimeException, Any] =
    EitherT.leftT[Future, Any](new RuntimeException("BOOOM!"))

  "EitherMatchers" should {
    "match a Right successfully with right " in {
      rightTest mustBeRight { toTest =>
        toTest.value2 must be("BigMoby")
      }
    }

    "match a Left successfully with left " in {
      leftTest mustBeLeft { ex =>
        ex.getMessage must be("BOOOM!")
      }
    }

    "match eitherT successfully with right " in {
      rightFromEitherTTest mustCompleteWithARight { toTest =>
        toTest.value2 must be("BigMoby")
      }
    }

    "match eitherT fail with wrong right " in {
      rightFromEitherTTest mustCompleteWithARight { toTest =>
        toTest.value2 mustNot be("CiccioPasticcio")
      }
    }

    "match eitherT successfully with left " in {
      leftFromEitherTTest mustCompleteWithALeft { ex: RuntimeException =>
        ex.getMessage must be("BOOOM!")
      }
    }

  }

}
