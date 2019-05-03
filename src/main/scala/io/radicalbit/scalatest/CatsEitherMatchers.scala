package io.radicalbit.scalatest

import cats.data.EitherT
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Assertion, MustMatchers, Succeeded}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag
import scala.util.Either

final private[scalatest] class BeCatsLeftEitherMatcher[L](f: L => Assertion)
    extends Matcher[L Either _] {
  def apply(either: L Either _): MatchResult = {
    MatchResult(
      either.fold(f(_) == Succeeded, _ => false),
      s"'$either' did not contain an Left element matching '$f'.",
      s"'$either' contained an Left element matching '$f', but should not have."
    )
  }
}

final private[scalatest] class BeCatsRightEitherMatcher[R](f: R => Assertion)
    extends Matcher[Either[_, R]] {
  def apply(either: Either[_, R]): MatchResult = {
    MatchResult(
      either.fold(_ => false, f(_) == Succeeded),
      s"'$either' did not contain an Right element matching '$f'.",
      s"'$either' contained an Right element matching '$f', but should not have."
    )
  }
}

final private[scalatest] class BeCatsLeftEitherTMatcher[L](f: L => Assertion)(
  duration: FiniteDuration
) extends Matcher[EitherT[Future, L, _]] {

  def apply(eitherT: EitherT[Future, L, _]): MatchResult = {

    val left: Either[L, _] = Await.result(eitherT.value, duration)

    MatchResult(
      left.fold(f(_) == Succeeded, _ => false),
      s"'$left' did not contain an Left element matching '$f'.",
      s"'$left' contained an Left element matching '$f', but should not have."
    )
  }
}

final private[scalatest] class BeCatsRightEitherTMatcher[T](f: T => Assertion)(
  duration: FiniteDuration
) extends Matcher[EitherT[Future, _, T]] {

  def apply(eitherT: EitherT[Future, _, T]): MatchResult = {

    val left: Either[_, T] = Await.result(eitherT.value, duration)

    MatchResult(
      left.fold(_ => false, f(_) == Succeeded),
      s"'$left' did not contain an Right element matching '$f'.",
      s"'$left' contained an Right element matching '$f', but should not have."
    )
  }
}

object CatsEitherMatchers {
  implicit class EnrichEither[L: ClassTag, R: ClassTag](in: Either[L, R])
      extends MustMatchers {

    def mustBeRight(f: R => Assertion): Assertion =
      in must new BeCatsRightEitherMatcher[R](f)

    def mustBeLeft(f: L => Assertion): Assertion =
      in must new BeCatsLeftEitherMatcher[L](f)
  }

  implicit class EnrichEitherT[L: ClassTag, R: ClassTag](
    in: EitherT[Future, L, R]
  )(implicit duration: FiniteDuration) {

    def mustCompleteWithARight(f: R => Assertion): MatchResult = {
      new BeCatsRightEitherTMatcher[R](f)(duration)(in)
    }

    def mustCompleteWithALeft[T](f: L => Assertion): MatchResult = {
      new BeCatsLeftEitherTMatcher[L](f)(duration)(in)
    }
  }
}
