package learnfp.monad

import org.scalatest.{Matchers, WordSpecLike}

import learnfp.monad.MonadOps.toMonadOpsPure
import learnfp.monad.WriterInstance._
import learnfp.functor.WriterInstance._
import learnfp.functor.Writer
import learnfp.functor.Writer._
import learnfp.monoid.ListMonoid._


class WriterTest extends WordSpecLike with Matchers {
  "writer monad" should {
    "work" in {
      type WriterString[A] = Writer[List[String], A];
      val res = {
        for {
          x <- 1.pure[WriterString]
          _ <- tell(List(x.toString))
          y <- 2.pure[WriterString]
          _ <- tell(List(y.toString))
          z <- 3.pure[WriterString]
          _ <- tell(List(z.toString))
      } yield (x, y, z) }

      res.run() shouldBe (List("een", "twee", "drie"), (1,2,3))
    }
  }
}

