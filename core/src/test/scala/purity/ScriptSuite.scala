package purity

import purity.script.io.{ fail ⇒ failedScript, _ }

class ScriptSuite extends PuritySuite {

  /*
  {
    implicit val F = ListWrapper.monad
    implicit val eq0 = EitherT.catsDataEqForEitherT[ListWrapper, String, Either[String, Int]]
    implicit val eq1 = EitherT.catsDataEqForEitherT[EitherT[ListWrapper, String, ?], String, Int](eq0)

    Functor[EitherT[ListWrapper, String, ?]]
    Applicative[EitherT[ListWrapper, String, ?]]
    Monad[EitherT[ListWrapper, String, ?]]

    checkAll("EitherT[ListWrapper, String, Int]", MonadErrorTests[EitherT[ListWrapper, String, ?], String].monadError[Int, Int, Int])
    checkAll("MonadError[EitherT[List, ?, ?]]", SerializableTests.serializable(MonadError[EitherT[ListWrapper, String, ?], String]))
    //checkAll("ScriptT[IO, Unit, Int, ?]", MonadErrorTests[ScriptT[IO, Unit, Int, ?], Int].monadError[Int, Int, Int])
  }
  */

  test("Script leftMap identity") {
    forAll { x: Int ⇒
      failedScript(x).leftMap(identity) == failedScript(x)
    }
  }
}
