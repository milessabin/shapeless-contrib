package shapeless.contrib.scalacheck

import org.scalacheck.{Gen,Properties,Shrink,Test}
import org.scalacheck.Prop.forAll
import shapeless.Iso
case class ShrinkTest(one: String,
                      two: String)

object ShrinkSpec extends Properties("Shrink") {

  private def shrinkClosure[T : Shrink](x: T): Stream[T] = {
    val xs = Shrink.shrink[T](x)
    if(xs.isEmpty) xs
    else xs.append(xs.take(1).map(shrinkClosure[T]).flatten)
  }

  implicit val shrinkTestIso = Iso.hlist(ShrinkTest.apply _, ShrinkTest.unapply _)
  val emptyShrinkTest = ShrinkTest("","")

  property("fromIso") = forAll {(shrinkMe: ShrinkTest) ⇒
    shrinkMe == emptyShrinkTest || shrinkClosure(shrinkMe).contains(emptyShrinkTest)
  }
}


