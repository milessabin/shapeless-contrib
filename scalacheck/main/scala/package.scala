package shapeless.contrib

import shapeless._

import scalaz._
import scalaz.syntax.apply._
import scalaz.scalacheck.ScalaCheckBinding._

import org.scalacheck.{Gen, Arbitrary, Shrink}

package object scalacheck {

  implicit def ArbitraryI: TypeClass[Arbitrary] = new TypeClass[Arbitrary] {

    def emptyProduct = Arbitrary(Gen.value(HNil))

    def product[F, T <: HList](f: Arbitrary[F], t: Arbitrary[T]) =
      (f |@| t) { _ :: _ }

    def derive[A, B](b: Arbitrary[B], ab: Iso[A, B]) =
      b.map(ab.from)

  }

  implicit def deriveArbitrary[F, G <: HList](implicit iso: Iso[F, G], hlistInst: TypeClass.HListInstance[Arbitrary, G]): Arbitrary[F] =
    TypeClass.deriveFromIso[Arbitrary, F, G]

  implicit def ShrinkI: TypeClass[Shrink] = new TypeClass[Shrink] {

    def emptyProduct = Shrink(_ ⇒ Stream.empty)

    def product[F, T <: HList](f: Shrink[F], t: Shrink[T]) = Shrink { case a :: b ⇒
      f.shrink(a).map( _ :: b) append
      t.shrink(b).map(a :: _)
    }

    def derive[A, B](b: Shrink[B], ab: Iso[A, B]) = Shrink { a ⇒
      b.shrink(ab.to(a)).map(ab.from)
    }
  }

  implicit def deriveShrink[F, G <: HList](implicit iso: Iso[F, G], hlistInst: TypeClass.HListInstance[Shrink, G]): Shrink[F] =
    TypeClass.deriveFromIso[Shrink, F, G]

}

// vim: expandtab:ts=2:sw=2
