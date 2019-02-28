package io.radicalbit.scalatest

import cats.data.EitherT
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{Assertion, MustMatchers, Succeeded}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag
import scala.util.Either

object CatsEitherMatchers {
  implicit class EnrichEither[L: ClassTag, R: ClassTag](in: Either[L, R])
      extends MustMatchers {

    def mustBeRight(f: R => Assertion): Assertion =
      in must new BeCatsRightEitherMatcher[R](f)

    def mustBeLeft(element: L): Assertion =
      in must new BeCatsLeftEitherMatcher[L](element)
  }

  implicit class EnrichEitherT[L: ClassTag, R: ClassTag](
    in: EitherT[Future, L, R]
  )(implicit duration: FiniteDuration) {
    def mustCompleteWithARight(
      f: R => Assertion
    ): Matcher[EitherT[Future, L, R]] = {
      new BeCatsRightEitherTMatcher[R](f)(duration)
    }

    def mustCompleteWithALeft[Throwable](
      f: Throwable => Assertion
    ): Matcher[EitherT[Future, Throwable, R]] = {
      new BeCatsLeftEitherTMatcher[Throwable](f)(duration)
    }
  }
}

final private[scalatest] class BeCatsLeftEitherMatcher[E](element: E)
    extends Matcher[E Either _] {
  def apply(either: E Either _): MatchResult = {
    MatchResult(
      either.fold(_ == element, _ => false),
      s"'$either' did not contain an Left element matching '$element'.",
      s"'$either' contained an Left element matching '$element', but should not have."
    )
  }
}

final private[scalatest] class BeCatsRightEitherMatcher[T](f: T => Assertion)
    extends Matcher[Either[_, T]] {
  def apply(either: Either[_, T]): MatchResult = {
    MatchResult(
      either.fold(_ => false, f(_) == Succeeded),
      s"'$either' did not contain an Right element matching '$f'.",
      s"'$either' contained an Right element matching '$f', but should not have."
    )
  }
}

final private[scalatest] class BeCatsLeftEitherTMatcher[Throwable](
  f: Throwable => Assertion
)(duration: FiniteDuration)
    extends Matcher[EitherT[Future, Throwable, _]] {
  def apply(eitherT: EitherT[Future, Throwable, _]): MatchResult = {

    val left: Either[Throwable, _] = Await.result(eitherT.value, duration)

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
