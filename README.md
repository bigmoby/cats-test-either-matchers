# Cats test EitherT and Either matchers

A simple utility matcher for Cats EitherT and plain Either type with Scalatest.

## Usage

##### With EitherT 
```
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
    
    "match eitherT successfully with left " in {
      leftTest mustCompleteWithALeft { ex: RuntimeException =>
        ex.getMessage must be("BOOOM!")
      }
    }
``` 

##### With Either type
```
  val rightTest: Either[_, Box] = Right(Box(3, "BigMoby"))

  val leftTest: Either[RuntimeException, _] = Left(
    new RuntimeException("BOOOM!")
  )
  
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
```