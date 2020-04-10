package tofu.bi

import cats.data.{IndexedStateT, State, StateT}
import cats.kernel.Monoid
import cats.{Applicative, Apply, Eval, FlatMap, Foldable, Functor, Id, Monad, Traverse}

/** constraint on binary type constructor
  * with fixed left argument
  */
trait RConstraint[+TC[_[_]], F[_, _]] {
  def supply[L]: TC[F[L, *]] = cachedAny.asInstanceOf[TC[F[L, *]]]

  protected val cachedAny:  TC[F[Any, *]]
}

trait RFunctor[F[_, _]] extends RConstraint[Functor, F] { self =>
  def map[L, A, B](fa: F[L, A])(f: A => B): F[L, B]

  override protected val cachedAny: FunctorImpl[Any] = new FunctorImpl[Any] {}

  trait FunctorImpl[L] extends Functor[F[L, *]]  {
    override def map[A, B](fa: F[L, A])(f: A => B): F[L, B] = self.map(fa)(f)
  }
}

trait RApply[F[_, _]] extends RConstraint[Apply, F] with RFunctor[F] { self =>
  def map2[L, A, B, C](fa: F[L, A], fb: F[L, B])(f: (A, B) => C): F[L, C]

  def ap[L, A, B](ff: F[L, A => B])(fa: F[L, A]): F[L, B] = map2(ff, fa)((_(_)))

  override protected val cachedAny: ApplyImpl[Any] = new ApplyImpl[Any] {}

  trait ApplyImpl[L] extends Apply[F[L, *]] with FunctorImpl[L] {
    override def ap[A, B](ff: F[L, A => B])(fa: F[L, A]): F[L, B] = self.ap(ff)(fa)

    override def map2[A, B, Z](fa: F[L, A], fb: F[L, B])(f: (A, B) => Z): F[L, Z] = self.map2(fa, fb)(f)
  }
}

trait RApplicative[F[_, _]] extends RConstraint[Applicative, F] with RApply[F] { self =>
  def pure[L, X](x: X): F[L, X]

  override protected val cachedAny: ApplicativeImpl[Any] = new ApplicativeImpl[Any] {}

  trait ApplicativeImpl[L] extends Applicative[F[L, *]] with ApplyImpl[L] {
    override def pure[A](x: A): F[L, A] = self.pure(x)
  }
}

trait RFoldable[F[_, _]] extends RConstraint[Foldable, F] { self =>
  def foldRightL[L, A, B](fa: F[L, A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]

  def foldLeft[L, A, B](fa: F[L, A])(b: B)(f: (B, A) => B): B = {
    type Go = B => Eval[B]
    def step(a: A, lg: Eval[Go]): Eval[Go] = Eval.always(b => lg.flatMap(g => g(f(b, a))))
    val init: Eval[Go]                     = Eval.now(Eval.now)
    foldRightL(fa)(init)(step).value(b).value
  }

  def foldMap[L, A, B](fa: F[L, A])(b: B)(f: A => B)(implicit B: Monoid[B]): B =
    foldLeft[L, A, B](fa)(B.empty)((b, a) => B.combine(b, f(a)))

  override protected val cachedAny: FoldableImpl[Any] = new FoldableImpl[Any] {}

  trait FoldableImpl[L] extends Foldable[F[L, *]] {
    override def foldLeft[A, B](fa: F[L, A], b: B)(f: (B, A) => B): B =
      self.foldLeft(fa)(b)(f)

    override def foldRight[A, B](fa: F[L, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      self.foldRightL(fa)(lb)(f)
  }
}

//trait RTraverse[F[_, _]] extends RConstraint[Traverse, F] with RFunctor[F] with RFoldable[F] { self =>
//  def traverse[G[_]: Applicative, L, A, B](fa: F[L, A])(f: A => G[B]): G[F[L, B]]
//
//  override def map[L, A, B](fa: F[L, A])(f: A => B): F[L, B] = traverse[Id, L, A, B](fa)(f)
//
//  def foldRightL[L, A, B](fa: F[L, A])(lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
//    traverse(fa)(a => StateT[Eval, Eval[B], Unit](lb => Eval.always(f(a, lb), ()))).runS(lb).flatMap(x => x)
//
//  override protected val cachedAny: Impl[Any] = new Impl[Any] {}
//
//  trait Impl[L] extends Traverse[F[L, *]] with super.Impl[L] {
//    def traverse[G[_]: Applicative, A, B](fa: F[L, A])(f: A => G[B]): G[F[L, B]] =
//      self.traverse(fa)(f)
//  }
//}

trait RFlatMap[F[_, _]] extends RConstraint[FlatMap, F] with RApply[F] { self =>
  def flatMap[L, A, B](fa: F[L, A])(f: A => F[L, B]): F[L, B]

  def tailRecM[L, A, B](a: A)(f: A => F[L, Either[A, B]]): F[L, B]

  def map2[L, A, B, C](fa: F[L, A], fb: F[L, B])(f: (A, B) => C): F[L, C] =
    flatMap(fa)(a => map(fb)(f(a, _)))

  override protected val cachedAny: Impl[Any] = new Impl[Any] {}

  trait Impl[L] extends FlatMap[F[L, *]] with super.Impl[L] {
    def flatMap[A, B](fa: F[L, A])(f: A => F[L, B]): F[L, B] = self.flatMap(fa)(f)

    def tailRecM[A, B](a: A)(f: A => F[L, Either[A, B]]): F[L, B] = self.tailRecM(a)(f)
  }
}

//trait RMonad[F[_, _]] extends RConstraint[Monad, F] with RApplicative[F] with RFlatMap[F] { self =>
//
//  override protected val cachedAny: Impl[Any] = super[RApplicative].pure(1)
//
//  trait Impl[L] extends Monad[F[L, *]] with super.Impl[L] {}
//}
