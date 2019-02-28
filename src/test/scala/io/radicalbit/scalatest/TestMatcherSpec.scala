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

  case class Contenitore(valore1: Int, valore2: String)

  val rightTest: EitherT[Future, Throwable, Contenitore] =
    EitherT.pure[Future, Throwable](Contenitore(3, "BigMoby"))

  val leftTest: EitherT[Future, RuntimeException, Any] =
    EitherT.leftT[Future, Any](new RuntimeException("BOOOM!"))

  "EitherMatchers" should {
    "match eitherT successfully with right " in {
      rightTest mustCompleteWithARight { daTestare =>
        daTestare.valore2 must be("BigMoby")
      }
    }

    "match eitherT fail with wrong right " in {
      rightTest mustCompleteWithARight { daTestare =>
        daTestare.valore2 mustNot be("CiccioPasticcio")
      }
    }

    "match eitherT successfully with left " in {
      leftTest mustCompleteWithALeft { ex: RuntimeException =>
        ex.getMessage must be("BOOOM!")
      }
    }

  }

}
